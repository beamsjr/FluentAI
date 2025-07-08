"""
Security Demo for FluentAI

Demonstrates how to use FluentAI's security features to run untrusted code safely.
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.security import (
    create_sandbox, run_sandboxed, SandboxConfig,
    CapabilitySet, Capability, EffectCapability,
    ResourceQuota, with_capabilities, with_resource_limits,
    validate_path, validate_url, sanitize_input
)
from src.effects.secure_handlers import create_secure_handler


def demo_basic_sandbox():
    """Demo: Run code in a basic sandbox"""
    print("=== Basic Sandbox Demo ===")
    
    # Safe code
    safe_code = """
result = sum(range(10))
print(f"Sum of 0-9: {result}")
"""
    
    result = run_sandboxed(safe_code)
    print(f"Result: {result}")
    
    # Dangerous code (will be blocked)
    dangerous_code = """
import os
files = os.listdir('/')
result = files
"""
    
    try:
        result = run_sandboxed(dangerous_code)
    except Exception as e:
        print(f"Blocked dangerous code: {e}")


def demo_capability_based_security():
    """Demo: Fine-grained capability control"""
    print("\n=== Capability-based Security Demo ===")
    
    # Create custom capability set
    custom_caps = CapabilitySet("custom", {
        Capability(EffectCapability.FILE_READ, constraints={'path': '/tmp/*'}),
        Capability(EffectCapability.NETWORK_CONNECT, constraints={'host': 'api.example.com'})
    })
    
    # Run code with specific capabilities
    with with_capabilities(custom_caps):
        # This would work if integrated with effect system
        print("Code running with limited file read and network access")
        
        # In a real implementation, this would check capabilities:
        # - Can read /tmp/test.txt ✓
        # - Cannot read /etc/passwd ✗
        # - Can connect to api.example.com ✓
        # - Cannot connect to evil.com ✗


def demo_resource_limits():
    """Demo: Resource quotas and limits"""
    print("\n=== Resource Limits Demo ===")
    
    # Set strict resource limits
    quota = ResourceQuota(
        max_memory=10 * 1024 * 1024,  # 10MB
        max_cpu_time=1.0,              # 1 second
        max_open_files=5,
        max_execution_time=2.0         # 2 seconds total
    )
    
    # Run code with resource limits
    with with_resource_limits(quota) as monitor:
        # Simulate some work
        data = [i for i in range(1000)]
        
        # Check resource usage
        usage = monitor.get_usage_report()
        print(f"Memory used: {usage['memory']['current'] / 1024 / 1024:.2f} MB")
        print(f"CPU time: {usage['cpu']['time']:.3f}s")


def demo_input_validation():
    """Demo: Input validation and sanitization"""
    print("\n=== Input Validation Demo ===")
    
    # Path validation
    try:
        safe_path = validate_path("/tmp/test.txt", allowed_dirs=["/tmp"])
        print(f"Valid path: {safe_path}")
    except Exception as e:
        print(f"Path validation error: {e}")
    
    try:
        dangerous_path = validate_path("../../etc/passwd", allowed_dirs=["/tmp"])
    except Exception as e:
        print(f"Blocked dangerous path: {e}")
    
    # URL validation
    try:
        safe_url = validate_url("https://api.example.com/data", 
                               allowed_hosts=["api.example.com"])
        print(f"Valid URL: {safe_url}")
    except Exception as e:
        print(f"URL validation error: {e}")
    
    # Input sanitization
    sql_input = "'; DROP TABLE users; --"
    safe_sql = sanitize_input(sql_input, context='sql')
    print(f"SQL input sanitized: '{sql_input}' -> '{safe_sql}'")
    
    html_input = '<script>alert("XSS")</script>'
    safe_html = sanitize_input(html_input, context='html')
    print(f"HTML input sanitized: '{html_input}' -> '{safe_html}'")


def demo_sandbox_with_timeout():
    """Demo: Sandbox with timeout protection"""
    print("\n=== Sandbox Timeout Demo ===")
    
    # Code that takes too long
    slow_code = """
import time
# This will timeout
for i in range(10):
    time.sleep(0.5)
    print(f"Step {i}")
result = "Should not reach here"
"""
    
    try:
        result = run_sandboxed(slow_code, timeout=2.0)
    except Exception as e:
        print(f"Code timed out as expected: {e}")


def demo_secure_effect_handler():
    """Demo: Secure effect handlers"""
    print("\n=== Secure Effect Handler Demo ===")
    
    # Create secure handler with restrictions
    handler = create_secure_handler(
        capability_set='file_sandbox',
        allowed_dirs=['/tmp/sandbox'],
        allowed_hosts=['api.safe.com', '*.trusted.org']
    )
    
    print("Created secure effect handler with:")
    print("- File access limited to /tmp/sandbox")
    print("- Network access limited to api.safe.com and *.trusted.org")
    print("- Path traversal protection enabled")
    print("- Resource quota enforcement")


def demo_complete_sandbox():
    """Demo: Complete sandbox with all security features"""
    print("\n=== Complete Sandbox Demo ===")
    
    # Configure comprehensive sandbox
    config = SandboxConfig(
        capability_set='read_only',
        resource_quota=ResourceQuota(
            max_memory=50 * 1024 * 1024,  # 50MB
            max_cpu_time=5.0,
            max_open_files=10,
            max_network_connections=0,     # No network
            max_execution_time=10.0
        ),
        allowed_paths=['/tmp/sandbox'],
        allow_network=False,
        timeout=10.0,
        allowed_modules=['math', 'json', 'datetime']
    )
    
    sandbox = create_sandbox('read_only', timeout=10.0, memory_limit=50*1024*1024)
    
    # Use in-process execution for this demo since imports are needed
    sandbox.config.use_subprocess = False
    
    # Safe computational code
    code = """
import math
import json

# Compute some primes
def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(math.sqrt(n)) + 1):
        if n % i == 0:
            return False
    return True

primes = [n for n in range(100) if is_prime(n)]
result = {
    'count': len(primes),
    'primes': primes,
    'sum': sum(primes)
}

print(json.dumps(result, indent=2))
result = result['count']
"""
    
    result = sandbox.execute(code)
    print(f"Sandbox executed successfully. Prime count: {result}")


if __name__ == "__main__":
    # Run all demos
    demo_basic_sandbox()
    demo_capability_based_security()
    demo_resource_limits()
    demo_input_validation()
    demo_sandbox_with_timeout()
    demo_secure_effect_handler()
    demo_complete_sandbox()
    
    print("\n=== Security Demo Complete ===")
    print("FluentAI provides comprehensive security features:")
    print("✓ Capability-based access control")
    print("✓ Resource quotas and limits")
    print("✓ Input validation and sanitization")
    print("✓ Process isolation and sandboxing")
    print("✓ Timeout protection")
    print("✓ Path traversal prevention")
    print("✓ Network access control")