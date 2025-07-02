"""
Security-enhanced effect handlers

Integrates capability checking and resource limits with effect handlers.
"""

from typing import Any, List, Dict, Optional
import os

from . import EffectHandler, EffectRequest, EffectResult
from ..core.ast import EffectType
from .handlers import IOHandler, StateHandler, NetworkHandler
from ..security.capabilities import (
    EffectCapability, require_capability, check_capability
)
from ..security.validators import (
    validate_path, validate_url, PathValidator, URLValidator,
    ValidationError
)
from ..security.limits import check_resource_operation


class SecureIOHandler(IOHandler):
    """IO handler with security checks"""
    
    def __init__(self, 
                 path_validator: Optional[PathValidator] = None,
                 allowed_dirs: Optional[List[str]] = None):
        super().__init__()
        self.path_validator = path_validator or PathValidator(
            allowed_dirs=allowed_dirs
        )
    
    def handle(self, effect: EffectRequest) -> EffectResult:
        """Handle IO effect with security checks"""
        op = effect.operation
        args = effect.args
        
        # Print operations
        if op == "print":
            # Check capability
            if not check_capability(EffectCapability.UI_RENDER):
                # Silently ignore if no capability
                return EffectResult(value=None)
            
            # Check resource usage
            output = ' '.join(str(arg) for arg in args)
            try:
                check_resource_operation('file_write', len(output))
            except Exception:
                return EffectResult(value=None)
            
            # Delegate to parent
            return super().handle(effect)
        
        # File operations
        elif op == "open-file":
            path, mode = args[0], args[1] if len(args) > 1 else 'r'
            
            # Validate path
            try:
                validated_path = self.path_validator.validate(path)
            except ValidationError as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "security-error", [str(e)])
                    ]
                )
            
            # Check capabilities based on mode
            if 'r' in mode:
                require_capability(EffectCapability.FILE_READ, path=str(validated_path))
            if 'w' in mode or 'a' in mode:
                require_capability(EffectCapability.FILE_WRITE, path=str(validated_path))
            if 'x' in mode:
                require_capability(EffectCapability.FILE_EXECUTE, path=str(validated_path))
            
            # Check resource limits
            check_resource_operation('file_open')
            
            # Open with validated path
            effect.args = [str(validated_path), mode]
            return super().handle(effect)
        
        elif op == "read-file":
            file_id = args[0]
            size = args[1] if len(args) > 1 else -1
            
            # Check capability (already checked on open, but double-check)
            require_capability(EffectCapability.FILE_READ)
            
            # Check resource limits if reading specific size
            if size > 0:
                check_resource_operation('file_read', size)
            
            return super().handle(effect)
        
        elif op == "write-file":
            file_id = args[0]
            data = args[1]
            
            # Check capability
            require_capability(EffectCapability.FILE_WRITE)
            
            # Check resource limits
            data_size = len(data) if isinstance(data, (str, bytes)) else 0
            check_resource_operation('file_write', data_size)
            
            return super().handle(effect)
        
        elif op == "delete-file":
            path = args[0]
            
            # Validate path
            try:
                validated_path = self.path_validator.validate(path)
            except ValidationError as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "security-error", [str(e)])
                    ]
                )
            
            # Check capability
            require_capability(EffectCapability.FILE_DELETE, path=str(validated_path))
            
            # Delete with validated path
            effect.args = [str(validated_path)]
            return super().handle(effect)
        
        # Default: delegate to parent
        return super().handle(effect)


class SecureNetworkHandler(NetworkHandler):
    """Network handler with security checks"""
    
    def __init__(self,
                 url_validator: Optional[URLValidator] = None,
                 allowed_hosts: Optional[List[str]] = None,
                 **kwargs):
        super().__init__(**kwargs)
        self.url_validator = url_validator or URLValidator(
            allowed_hosts=allowed_hosts or self.allowed_hosts
        )
    
    def handle(self, effect: EffectRequest) -> EffectResult:
        """Handle network effect with security checks"""
        op = effect.operation
        
        if op.startswith("http-"):
            # Validate URL
            url = effect.args[0]
            try:
                validated_url = self.url_validator.validate(url)
            except ValidationError as e:
                return EffectResult(
                    value={
                        'status': 0,
                        'error': str(e),
                        'body': None,
                        'headers': {}
                    }
                )
            
            # Check capability
            require_capability(EffectCapability.NETWORK_CONNECT, url=validated_url)
            
            # Check resource limits
            check_resource_operation('network_connect')
            
            # Update URL to validated version
            effect.args = [validated_url] + list(effect.args[1:])
            
            # Handle the request
            result = super().handle(effect)
            
            # Track data transfer
            if result.value and isinstance(result.value, dict):
                body = result.value.get('body', '')
                if body:
                    data_size = len(body) if isinstance(body, (str, bytes)) else 0
                    check_resource_operation('network_transfer', data_size)
            
            return result
        
        elif op == "websocket-connect":
            url = effect.args[0]
            
            # Validate URL
            try:
                validated_url = self.url_validator.validate(url)
            except ValidationError as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "security-error", [str(e)])
                    ]
                )
            
            # Check capability
            require_capability(EffectCapability.NETWORK_CONNECT, url=validated_url)
            check_resource_operation('network_connect')
            
            # Update URL
            effect.args = [validated_url] + list(effect.args[1:])
            return super().handle(effect)
        
        # Default: delegate to parent
        return super().handle(effect)


class SecureStateHandler(StateHandler):
    """State handler with security checks"""
    
    def handle(self, effect: EffectRequest) -> EffectResult:
        """Handle state effect with security checks"""
        # State operations are generally safe, but we can add
        # memory limits for large state updates
        op = effect.operation
        
        if op in ["set", "update"]:
            # Check size of value being stored
            value = effect.args[1] if len(effect.args) > 1 else None
            if value:
                # Rough estimate of memory usage
                import sys
                size = sys.getsizeof(value)
                check_resource_operation('memory_allocate', size)
        
        return super().handle(effect)


class SecureEffectHandler(EffectHandler):
    """Composite handler that adds security to all effects"""
    
    def __init__(self,
                 allowed_dirs: Optional[List[str]] = None,
                 allowed_hosts: Optional[List[str]] = None):
        self.io_handler = SecureIOHandler(allowed_dirs=allowed_dirs)
        self.network_handler = SecureNetworkHandler(allowed_hosts=allowed_hosts)
        self.state_handler = SecureStateHandler()
        
        # Map effect types to handlers
        self.handlers = {
            EffectType.IO: self.io_handler,
            EffectType.NETWORK: self.network_handler,
            EffectType.STATE: self.state_handler,
        }
    
    def can_handle(self, effect: EffectRequest) -> bool:
        """Check if this handler can handle the effect"""
        return effect.effect_type in self.handlers
    
    def handle(self, effect: EffectRequest) -> EffectResult:
        """Route effect to appropriate secure handler"""
        handler = self.handlers.get(effect.effect_type)
        
        if handler:
            return handler.handle(effect)
        else:
            # Unknown effect type - check if we have capability
            # This is a catch-all for custom effects
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(
                        EffectType.ERROR, 
                        "unknown-effect", 
                        [f"Unknown effect type: {effect.effect_type}"]
                    )
                ]
            )


def create_secure_handler(capability_set: str = 'read_only',
                         allowed_dirs: Optional[List[str]] = None,
                         allowed_hosts: Optional[List[str]] = None) -> SecureEffectHandler:
    """Create a secure effect handler with given capabilities"""
    # The capability set is enforced by the security context,
    # this handler just adds the checks
    return SecureEffectHandler(
        allowed_dirs=allowed_dirs,
        allowed_hosts=allowed_hosts
    )