"""
ClaudeLang Security and Sandboxing Module

Provides comprehensive security features including:
- Capability-based security for effects
- Resource limits and quotas
- Path traversal protection
- Input validation
- Sandboxing for untrusted code
"""

from .capabilities import (
    Capability, CapabilitySet, CapabilityManager,
    SecurityContext, SecurityPolicy, EffectCapability,
    check_capability, require_capability, with_capabilities
)
from .sandbox import (
    Sandbox, SandboxConfig,
    create_sandbox, run_sandboxed
)
from .validators import (
    validate_path, validate_url, validate_host,
    sanitize_input, PathValidator, URLValidator
)
from .limits import (
    ResourceMonitor, ResourceQuota, QuotaExceeded,
    MemoryLimit, CPULimit, NetworkLimit,
    with_resource_limits
)

__all__ = [
    # Capabilities
    'Capability', 'CapabilitySet', 'CapabilityManager',
    'SecurityContext', 'SecurityPolicy', 'EffectCapability',
    'check_capability', 'require_capability', 'with_capabilities',
    
    # Sandboxing
    'Sandbox', 'SandboxConfig',
    'create_sandbox', 'run_sandboxed',
    
    # Validators
    'validate_path', 'validate_url', 'validate_host',
    'sanitize_input', 'PathValidator', 'URLValidator',
    
    # Resource Limits
    'ResourceMonitor', 'ResourceQuota', 'QuotaExceeded',
    'MemoryLimit', 'CPULimit', 'NetworkLimit', 'with_resource_limits'
]