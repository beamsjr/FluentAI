"""
Capability-based Security System

Implements fine-grained permission control for effects and operations.
"""

from typing import Set, Dict, Any, Optional, List, Callable
from dataclasses import dataclass, field
from enum import Enum, auto
import fnmatch
from contextlib import contextmanager


class EffectCapability(Enum):
    """Fine-grained effect capabilities"""
    # File System
    FILE_READ = auto()
    FILE_WRITE = auto()
    FILE_DELETE = auto()
    FILE_EXECUTE = auto()
    
    # Network
    NETWORK_CONNECT = auto()
    NETWORK_LISTEN = auto()
    NETWORK_DNS = auto()
    
    # Process
    PROCESS_SPAWN = auto()
    PROCESS_SIGNAL = auto()
    
    # System
    SYSTEM_TIME = auto()
    SYSTEM_ENV = auto()
    SYSTEM_INFO = auto()
    
    # Memory
    MEMORY_ALLOCATE = auto()
    MEMORY_MAP = auto()
    
    # UI
    UI_CREATE = auto()
    UI_RENDER = auto()
    UI_INPUT = auto()


class Capability:
    """A single capability with optional constraints"""
    def __init__(self, capability: EffectCapability, constraints: Optional[Dict[str, Any]] = None):
        self.capability = capability
        self.constraints = constraints or {}
    
    def __hash__(self):
        """Make capability hashable"""
        # Use capability enum and sorted constraints for hash
        constraint_items = tuple(sorted(self.constraints.items()))
        return hash((self.capability, constraint_items))
    
    def __eq__(self, other):
        """Check equality"""
        if not isinstance(other, Capability):
            return False
        return (self.capability == other.capability and 
                self.constraints == other.constraints)
    
    def allows(self, operation: str, context: Dict[str, Any]) -> bool:
        """Check if this capability allows the operation"""
        # Check constraints
        for key, pattern in self.constraints.items():
            if key not in context:
                return False
            
            value = context[key]
            
            # Pattern matching for strings
            if isinstance(pattern, str) and isinstance(value, str):
                if not fnmatch.fnmatch(value, pattern):
                    return False
            # Exact match for other types
            elif value != pattern:
                return False
        
        return True


@dataclass
class CapabilitySet:
    """A set of capabilities that can be granted together"""
    name: str
    capabilities: Set[Capability] = field(default_factory=set)
    inherit_from: Optional[List['CapabilitySet']] = None
    
    def __post_init__(self):
        """Inherit capabilities from parent sets"""
        if self.inherit_from:
            for parent in self.inherit_from:
                self.capabilities.update(parent.capabilities)
    
    def has_capability(self, cap: EffectCapability, context: Dict[str, Any]) -> bool:
        """Check if this set has a capability for the given context"""
        for capability in self.capabilities:
            if capability.capability == cap and capability.allows("", context):
                return True
        return False
    
    def add_capability(self, cap: Capability):
        """Add a capability to this set"""
        self.capabilities.add(cap)
    
    def remove_capability(self, cap: Capability):
        """Remove a capability from this set"""
        self.capabilities.discard(cap)


# Predefined capability sets
CAPABILITY_SETS = {
    'none': CapabilitySet('none'),
    
    'read_only': CapabilitySet('read_only', {
        Capability(EffectCapability.FILE_READ),
        Capability(EffectCapability.SYSTEM_TIME),
        Capability(EffectCapability.SYSTEM_INFO),
    }),
    
    'network_client': CapabilitySet('network_client', {
        Capability(EffectCapability.NETWORK_CONNECT),
        Capability(EffectCapability.NETWORK_DNS),
    }),
    
    'file_sandbox': CapabilitySet('file_sandbox', {
        Capability(EffectCapability.FILE_READ, constraints={'path': '/tmp/sandbox/*'}),
        Capability(EffectCapability.FILE_WRITE, constraints={'path': '/tmp/sandbox/*'}),
    }),
    
    'ui_basic': CapabilitySet('ui_basic', {
        Capability(EffectCapability.UI_CREATE),
        Capability(EffectCapability.UI_RENDER),
        Capability(EffectCapability.UI_INPUT),
    }),
    
    'trusted': CapabilitySet('trusted', {
        Capability(cap) for cap in EffectCapability
    }),
}


@dataclass
class SecurityContext:
    """Security context for code execution"""
    capability_set: CapabilitySet
    resource_limits: Dict[str, Any] = field(default_factory=dict)
    audit_log: List[Dict[str, Any]] = field(default_factory=list)
    
    def check_capability(self, cap: EffectCapability, context: Dict[str, Any]) -> bool:
        """Check if an operation is allowed"""
        allowed = self.capability_set.has_capability(cap, context)
        
        # Audit the check
        self.audit_log.append({
            'capability': cap.name,
            'context': context,
            'allowed': allowed,
            'timestamp': _get_timestamp()
        })
        
        return allowed
    
    def require_capability(self, cap: EffectCapability, context: Dict[str, Any]):
        """Require a capability, raise if not allowed"""
        if not self.check_capability(cap, context):
            raise SecurityError(f"Capability {cap.name} denied for context {context}")


class SecurityPolicy:
    """Global security policy manager"""
    
    def __init__(self):
        self.policies: Dict[str, CapabilitySet] = {}
        self.default_policy = CAPABILITY_SETS['read_only']
        self.enforcement_hooks: List[Callable] = []
    
    def register_policy(self, name: str, capability_set: CapabilitySet):
        """Register a named security policy"""
        self.policies[name] = capability_set
    
    def get_policy(self, name: str) -> CapabilitySet:
        """Get a security policy by name"""
        return self.policies.get(name, self.default_policy)
    
    def create_context(self, policy_name: str) -> SecurityContext:
        """Create a security context from a policy"""
        policy = self.get_policy(policy_name)
        return SecurityContext(capability_set=policy)
    
    def add_enforcement_hook(self, hook: Callable):
        """Add a hook that's called on capability checks"""
        self.enforcement_hooks.append(hook)
    
    @contextmanager
    def enforce_policy(self, policy_name: str):
        """Context manager to enforce a security policy"""
        context = self.create_context(policy_name)
        
        # Install context
        token = _install_security_context(context)
        
        try:
            yield context
        finally:
            # Remove context
            _uninstall_security_context(token)


class CapabilityManager:
    """Manages capability checking for the runtime"""
    
    def __init__(self):
        self.current_context: Optional[SecurityContext] = None
        self.context_stack: List[SecurityContext] = []
    
    def push_context(self, context: SecurityContext):
        """Push a new security context"""
        if self.current_context:
            self.context_stack.append(self.current_context)
        self.current_context = context
    
    def pop_context(self) -> Optional[SecurityContext]:
        """Pop the current security context"""
        old_context = self.current_context
        
        if self.context_stack:
            self.current_context = self.context_stack.pop()
        else:
            self.current_context = None
        
        return old_context
    
    def check_capability(self, cap: EffectCapability, context: Dict[str, Any]) -> bool:
        """Check capability in current context"""
        if not self.current_context:
            # No security context means no capabilities
            return False
        
        return self.current_context.check_capability(cap, context)
    
    def require_capability(self, cap: EffectCapability, context: Dict[str, Any]):
        """Require capability in current context"""
        if not self.current_context:
            raise SecurityError("No security context active")
        
        self.current_context.require_capability(cap, context)


# Global capability manager instance
_capability_manager = CapabilityManager()


def check_capability(cap: EffectCapability, **context) -> bool:
    """Check if the current context has a capability"""
    return _capability_manager.check_capability(cap, context)


def require_capability(cap: EffectCapability, **context):
    """Require a capability in the current context"""
    _capability_manager.require_capability(cap, context)


@contextmanager
def with_capabilities(capability_set: CapabilitySet):
    """Execute code with specific capabilities"""
    context = SecurityContext(capability_set=capability_set)
    _capability_manager.push_context(context)
    
    try:
        yield context
    finally:
        _capability_manager.pop_context()


# Helper functions
def _get_timestamp():
    """Get current timestamp for audit logs"""
    import time
    return time.time()


def _install_security_context(context: SecurityContext) -> Any:
    """Install a security context (returns token for removal)"""
    _capability_manager.push_context(context)
    return context


def _uninstall_security_context(token: Any):
    """Uninstall a security context"""
    _capability_manager.pop_context()


class SecurityError(Exception):
    """Security violation error"""
    pass