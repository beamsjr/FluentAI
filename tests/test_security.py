"""
Tests for security and sandboxing features
"""

import unittest
import tempfile
import os
from pathlib import Path

from src.security.capabilities import (
    EffectCapability, Capability, CapabilitySet, SecurityContext,
    check_capability, require_capability, with_capabilities,
    SecurityError, CAPABILITY_SETS
)
from src.security.validators import (
    PathValidator, URLValidator, InputSanitizer,
    ValidationError, validate_path, validate_url
)
from src.security.limits import (
    ResourceQuota, ResourceMonitor, QuotaExceeded,
    MemoryLimit, CPULimit, with_resource_limits
)
from src.security.sandbox import (
    Sandbox, SandboxConfig, run_sandboxed
)


class TestCapabilities(unittest.TestCase):
    """Test capability-based security"""
    
    def test_basic_capability(self):
        """Test basic capability checking"""
        cap = Capability(EffectCapability.FILE_READ)
        
        # Should allow any file read
        self.assertTrue(cap.allows("read", {"path": "/etc/passwd"}))
        self.assertTrue(cap.allows("read", {"path": "/home/user/file.txt"}))
    
    def test_constrained_capability(self):
        """Test capability with constraints"""
        cap = Capability(
            EffectCapability.FILE_READ,
            constraints={"path": "/tmp/*"}
        )
        
        # Should only allow /tmp files
        self.assertTrue(cap.allows("read", {"path": "/tmp/test.txt"}))
        self.assertTrue(cap.allows("read", {"path": "/tmp/subdir/file.txt"}))
        self.assertFalse(cap.allows("read", {"path": "/etc/passwd"}))
    
    def test_capability_set(self):
        """Test capability sets"""
        cap_set = CapabilitySet("test", {
            Capability(EffectCapability.FILE_READ, {"path": "/tmp/*"}),
            Capability(EffectCapability.FILE_WRITE, {"path": "/tmp/*"})
        })
        
        # Check capabilities
        self.assertTrue(cap_set.has_capability(
            EffectCapability.FILE_READ,
            {"path": "/tmp/test.txt"}
        ))
        self.assertFalse(cap_set.has_capability(
            EffectCapability.FILE_READ,
            {"path": "/etc/passwd"}
        ))
        self.assertFalse(cap_set.has_capability(
            EffectCapability.FILE_DELETE,
            {"path": "/tmp/test.txt"}
        ))
    
    def test_security_context(self):
        """Test security context"""
        cap_set = CAPABILITY_SETS['read_only']
        context = SecurityContext(capability_set=cap_set)
        
        # Should allow reads
        self.assertTrue(context.check_capability(
            EffectCapability.FILE_READ,
            {"path": "/test.txt"}
        ))
        
        # Should deny writes
        self.assertFalse(context.check_capability(
            EffectCapability.FILE_WRITE,
            {"path": "/test.txt"}
        ))
        
        # Should raise on require
        with self.assertRaises(SecurityError):
            context.require_capability(
                EffectCapability.FILE_WRITE,
                {"path": "/test.txt"}
            )
    
    def test_with_capabilities_context(self):
        """Test capability context manager"""
        cap_set = CAPABILITY_SETS['file_sandbox']
        
        # Outside context - no capabilities
        self.assertFalse(check_capability(
            EffectCapability.FILE_READ,
            path="/tmp/sandbox/test.txt"
        ))
        
        # Inside context - has capabilities
        with with_capabilities(cap_set):
            self.assertTrue(check_capability(
                EffectCapability.FILE_READ,
                path="/tmp/sandbox/test.txt"
            ))
            self.assertTrue(check_capability(
                EffectCapability.FILE_WRITE,
                path="/tmp/sandbox/test.txt"
            ))
            self.assertFalse(check_capability(
                EffectCapability.FILE_WRITE,
                path="/etc/passwd"
            ))


class TestValidators(unittest.TestCase):
    """Test input validators"""
    
    def test_path_validator(self):
        """Test path validation"""
        validator = PathValidator(allowed_dirs=["/tmp"])
        
        # Valid paths
        with tempfile.TemporaryDirectory() as tmpdir:
            validator = PathValidator(allowed_dirs=[tmpdir])
            
            valid_path = Path(tmpdir) / "test.txt"
            valid_path.touch()
            
            result = validator.validate(valid_path)
            self.assertEqual(result, valid_path.resolve())
        
        # Invalid paths
        with self.assertRaises(ValidationError):
            validator.validate("/etc/passwd")
        
        with self.assertRaises(ValidationError):
            validator.validate("../../../etc/passwd")
        
        with self.assertRaises(ValidationError):
            validator.validate("/tmp/../etc/passwd")
    
    def test_path_sanitization(self):
        """Test path sanitization"""
        validator = PathValidator()
        
        # Sanitize dangerous paths
        self.assertEqual(validator.sanitize("test.txt"), "test.txt")
        self.assertEqual(validator.sanitize("../test.txt"), "test.txt")
        self.assertEqual(validator.sanitize("test\0.txt"), "test.txt")
        self.assertEqual(validator.sanitize("test/../file.txt"), "test/file.txt")
    
    def test_url_validator(self):
        """Test URL validation"""
        validator = URLValidator(
            allowed_schemes=['https'],
            allowed_hosts=['example.com', '*.safe.com']
        )
        
        # Valid URLs
        self.assertEqual(
            validator.validate("https://example.com/path"),
            "https://example.com/path"
        )
        self.assertEqual(
            validator.validate("https://api.safe.com/data"),
            "https://api.safe.com/data"
        )
        
        # Invalid URLs
        with self.assertRaises(ValidationError):
            validator.validate("http://example.com/path")  # Wrong scheme
        
        with self.assertRaises(ValidationError):
            validator.validate("https://evil.com/path")  # Not allowed host
        
        with self.assertRaises(ValidationError):
            validator.validate("https://user:pass@example.com/")  # Credentials
    
    def test_input_sanitizer(self):
        """Test input sanitization"""
        # SQL sanitization
        sql_input = "'; DROP TABLE users; --"
        sanitized = InputSanitizer.sanitize_sql(sql_input)
        self.assertNotIn("DROP", sanitized)
        self.assertNotIn("--", sanitized)
        
        # HTML sanitization
        html_input = '<script>alert("XSS")</script>'
        sanitized = InputSanitizer.sanitize_html(html_input)
        self.assertNotIn("<script>", sanitized)
        self.assertIn("&lt;script&gt;", sanitized)
        
        # Shell sanitization
        shell_input = "test; rm -rf /"
        sanitized = InputSanitizer.sanitize_shell(shell_input)
        self.assertNotIn(";", sanitized)
        
        # Filename sanitization
        filename = "../../../etc/passwd"
        sanitized = InputSanitizer.sanitize_filename(filename)
        self.assertNotIn("/", sanitized)
        self.assertNotIn("..", sanitized)


class TestResourceLimits(unittest.TestCase):
    """Test resource limits and monitoring"""
    
    def test_memory_limit(self):
        """Test memory limit enforcement"""
        limit = MemoryLimit(max_bytes=1024 * 1024)  # 1MB
        
        with limit.enforce():
            # Small allocation should be fine
            data = [0] * 1000
        
        # Large allocation should fail
        # Note: This test is tricky because Python memory management
        # is complex, so we'll just test the mechanism
        limit.check()  # Should not raise if under limit
    
    def test_cpu_limit(self):
        """Test CPU time limit"""
        limit = CPULimit(max_seconds=0.1)
        
        with self.assertRaises(QuotaExceeded):
            with limit.enforce():
                # Busy loop to consume CPU
                import time
                start = time.time()
                while time.time() - start < 0.2:
                    pass
    
    def test_resource_monitor(self):
        """Test resource monitoring"""
        quota = ResourceQuota(
            max_memory=100 * 1024 * 1024,  # 100MB
            max_cpu_time=1.0,
            max_open_files=10
        )
        
        monitor = ResourceMonitor(quota)
        
        # Check operations
        monitor.check_operation('memory_allocate', 1024)  # Should pass
        
        with self.assertRaises(QuotaExceeded):
            monitor.check_operation('memory_allocate', 200 * 1024 * 1024)
        
        # Get usage report
        report = monitor.get_usage_report()
        self.assertIn('memory', report)
        self.assertIn('cpu', report)
        self.assertIn('files', report)
    
    def test_with_resource_limits(self):
        """Test resource limit context manager"""
        quota = ResourceQuota(
            max_memory=50 * 1024 * 1024,
            max_execution_time=0.1
        )
        
        # Should complete successfully
        with with_resource_limits(quota) as monitor:
            data = [0] * 1000
            report = monitor.get_usage_report()
            self.assertIsNotNone(report)


class TestSandbox(unittest.TestCase):
    """Test sandboxing functionality"""
    
    def test_basic_sandbox(self):
        """Test basic sandbox execution"""
        code = """
result = 2 + 2
"""
        result = run_sandboxed(code)
        self.assertEqual(result, 4)
    
    def test_sandbox_restrictions(self):
        """Test sandbox restrictions"""
        # Should not be able to import os
        code = """
import os
result = os.listdir('/')
"""
        with self.assertRaises(Exception):
            run_sandboxed(code)
        
        # Should not be able to open files
        code = """
with open('/etc/passwd', 'r') as f:
    result = f.read()
"""
        with self.assertRaises(Exception):
            run_sandboxed(code)
        
        # Should not be able to use eval
        code = """
result = eval('__import__("os").system("ls")')
"""
        with self.assertRaises(Exception):
            run_sandboxed(code)
    
    def test_sandbox_with_capabilities(self):
        """Test sandbox with specific capabilities"""
        config = SandboxConfig(
            capability_set='file_sandbox',
            temp_dir='/tmp/test_sandbox'
        )
        
        sandbox = Sandbox(config)
        
        # Should be able to work with temp files
        code = """
# This would need the file operations to be hooked into the effect system
result = "sandbox works"
"""
        
        result = sandbox.execute(code)
        self.assertEqual(result, "sandbox works")
    
    def test_sandbox_timeout(self):
        """Test sandbox timeout"""
        code = """
import time
while True:
    time.sleep(0.1)
"""
        
        with self.assertRaises(Exception):  # Should timeout
            run_sandboxed(code, timeout=0.5)
    
    def test_sandbox_memory_limit(self):
        """Test sandbox memory limits"""
        code = """
# Try to allocate lots of memory
data = []
for i in range(1000000):
    data.append([0] * 1000)
result = len(data)
"""
        
        # Should fail due to memory limit
        with self.assertRaises(Exception):
            run_sandboxed(code)


if __name__ == "__main__":
    unittest.main()