"""
Security patches for existing handlers

Adds basic security checks to existing handlers without breaking compatibility.
"""

import os
from pathlib import Path
from typing import Optional, List

from .handlers import IOHandler, NetworkHandler
from .base import EffectRequest, EffectResult, EffectType


def patch_io_handler_security(handler: IOHandler, 
                            allowed_dirs: Optional[List[str]] = None):
    """Add security checks to an existing IO handler"""
    
    # Save original handle method
    original_handle = handler.handle
    
    def secure_handle(effect: EffectRequest) -> EffectResult:
        """Handle with security checks"""
        op = effect.operation
        args = effect.args
        
        if op == "open-file":
            path = args[0]
            
            # Basic path traversal check
            if '..' in Path(path).parts:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(
                            EffectType.ERROR, 
                            "security-error", 
                            ["Path traversal detected"]
                        )
                    ]
                )
            
            # Check allowed directories if specified
            if allowed_dirs:
                try:
                    resolved_path = Path(path).resolve()
                    allowed = False
                    
                    for allowed_dir in allowed_dirs:
                        allowed_path = Path(allowed_dir).resolve()
                        try:
                            resolved_path.relative_to(allowed_path)
                            allowed = True
                            break
                        except ValueError:
                            continue
                    
                    if not allowed:
                        return EffectResult(
                            value=None,
                            secondary_effects=[
                                EffectRequest(
                                    EffectType.ERROR,
                                    "security-error",
                                    [f"Access to {path} not allowed"]
                                )
                            ]
                        )
                except Exception as e:
                    return EffectResult(
                        value=None,
                        secondary_effects=[
                            EffectRequest(
                                EffectType.ERROR,
                                "security-error",
                                [f"Invalid path: {e}"]
                            )
                        ]
                    )
        
        # Call original handler
        return original_handle(effect)
    
    # Replace handle method
    handler.handle = secure_handle
    
    # Mark as patched
    handler._security_patched = True
    
    return handler


def patch_network_handler_security(handler: NetworkHandler,
                                 additional_blocked_hosts: Optional[List[str]] = None):
    """Add additional security checks to network handler"""
    
    # Save original _is_host_allowed method
    original_is_host_allowed = handler._is_host_allowed
    
    def secure_is_host_allowed(host: str) -> bool:
        """Check host with additional security"""
        # Check original restrictions
        if not original_is_host_allowed(host):
            return False
        
        # Additional blocked hosts
        if additional_blocked_hosts:
            for blocked in additional_blocked_hosts:
                if blocked.startswith('*.'):
                    # Wildcard
                    if host.endswith(blocked[2:]):
                        return False
                elif host == blocked:
                    return False
        
        # Block local/private addresses
        dangerous_hosts = [
            'localhost', '127.0.0.1', '0.0.0.0',
            '::1', '169.254.169.254'  # AWS metadata service
        ]
        
        if host in dangerous_hosts:
            return False
        
        # Block private IP ranges
        import ipaddress
        try:
            ip = ipaddress.ip_address(host)
            if ip.is_private or ip.is_loopback or ip.is_link_local:
                return False
        except ValueError:
            # Not an IP address, that's ok
            pass
        
        return True
    
    # Replace method
    handler._is_host_allowed = secure_is_host_allowed
    
    # Mark as patched
    handler._security_patched = True
    
    return handler


def apply_default_security_patches():
    """Apply default security patches to the global handlers"""
    # This would be called on module initialization
    # to ensure all handlers have basic security
    pass