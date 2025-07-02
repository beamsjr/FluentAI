"""
Resource limits and quotas

Implements resource monitoring and enforcement for security.
"""

import time
import threading
import resource
import psutil
from typing import Dict, Any, Optional, Callable
from dataclasses import dataclass, field
from contextlib import contextmanager
from collections import defaultdict


class QuotaExceeded(Exception):
    """Raised when a resource quota is exceeded"""
    pass


@dataclass
class ResourceQuota:
    """Defines quotas for various resources"""
    # Memory limits (in bytes)
    max_memory: Optional[int] = None
    max_heap: Optional[int] = None
    
    # CPU limits
    max_cpu_time: Optional[float] = None  # seconds
    max_cpu_percent: Optional[float] = None  # percentage (0-100)
    
    # File system limits
    max_file_size: Optional[int] = None  # bytes
    max_open_files: Optional[int] = None
    max_disk_usage: Optional[int] = None  # bytes
    
    # Network limits
    max_network_connections: Optional[int] = None
    max_bandwidth_bytes: Optional[int] = None  # bytes per second
    max_data_transfer: Optional[int] = None  # total bytes
    
    # Process limits
    max_processes: Optional[int] = None
    max_threads: Optional[int] = None
    
    # Time limits
    max_execution_time: Optional[float] = None  # seconds


@dataclass
class ResourceUsage:
    """Tracks current resource usage"""
    memory_bytes: int = 0
    heap_bytes: int = 0
    cpu_time: float = 0.0
    cpu_percent: float = 0.0
    file_count: int = 0
    disk_bytes: int = 0
    network_connections: int = 0
    network_bytes: int = 0
    process_count: int = 0
    thread_count: int = 0
    start_time: float = field(default_factory=time.time)
    
    @property
    def execution_time(self) -> float:
        """Get total execution time"""
        return time.time() - self.start_time


class ResourceMonitor:
    """Monitors and enforces resource limits"""
    
    def __init__(self, quota: ResourceQuota):
        self.quota = quota
        self.usage = ResourceUsage()
        self.process = psutil.Process()
        self._monitoring = False
        self._monitor_thread: Optional[threading.Thread] = None
        self._violation_callbacks: List[Callable] = []
        self._lock = threading.Lock()
    
    def start_monitoring(self):
        """Start resource monitoring"""
        if self._monitoring:
            return
        
        self._monitoring = True
        self._monitor_thread = threading.Thread(
            target=self._monitor_loop,
            daemon=True
        )
        self._monitor_thread.start()
    
    def stop_monitoring(self):
        """Stop resource monitoring"""
        self._monitoring = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=1.0)
    
    def _monitor_loop(self):
        """Main monitoring loop"""
        while self._monitoring:
            try:
                self._update_usage()
                self._check_limits()
                time.sleep(0.1)  # Check every 100ms
            except Exception:
                # Don't let monitoring errors crash the thread
                pass
    
    def _update_usage(self):
        """Update current resource usage"""
        with self._lock:
            # Memory usage
            mem_info = self.process.memory_info()
            self.usage.memory_bytes = mem_info.rss
            self.usage.heap_bytes = mem_info.vms
            
            # CPU usage
            self.usage.cpu_percent = self.process.cpu_percent(interval=0.1)
            cpu_times = self.process.cpu_times()
            self.usage.cpu_time = cpu_times.user + cpu_times.system
            
            # File descriptors
            try:
                self.usage.file_count = len(self.process.open_files())
            except:
                pass
            
            # Network connections
            try:
                self.usage.network_connections = len(self.process.net_connections())
            except:
                pass
            
            # Threads
            self.usage.thread_count = self.process.num_threads()
    
    def _check_limits(self):
        """Check if any limits are exceeded"""
        violations = []
        
        with self._lock:
            # Memory limits
            if (self.quota.max_memory and 
                self.usage.memory_bytes > self.quota.max_memory):
                violations.append(('memory', self.usage.memory_bytes, 
                                 self.quota.max_memory))
            
            # CPU time limit
            if (self.quota.max_cpu_time and 
                self.usage.cpu_time > self.quota.max_cpu_time):
                violations.append(('cpu_time', self.usage.cpu_time, 
                                 self.quota.max_cpu_time))
            
            # CPU percentage limit
            if (self.quota.max_cpu_percent and 
                self.usage.cpu_percent > self.quota.max_cpu_percent):
                violations.append(('cpu_percent', self.usage.cpu_percent, 
                                 self.quota.max_cpu_percent))
            
            # Execution time limit
            if (self.quota.max_execution_time and 
                self.usage.execution_time > self.quota.max_execution_time):
                violations.append(('execution_time', self.usage.execution_time, 
                                 self.quota.max_execution_time))
            
            # File count limit
            if (self.quota.max_open_files and 
                self.usage.file_count > self.quota.max_open_files):
                violations.append(('open_files', self.usage.file_count, 
                                 self.quota.max_open_files))
        
        # Handle violations
        for resource, current, limit in violations:
            self._handle_violation(resource, current, limit)
    
    def _handle_violation(self, resource: str, current: Any, limit: Any):
        """Handle a resource limit violation"""
        # Call violation callbacks
        for callback in self._violation_callbacks:
            callback(resource, current, limit)
        
        # Raise exception to stop execution
        raise QuotaExceeded(
            f"Resource quota exceeded: {resource} = {current} (limit: {limit})"
        )
    
    def add_violation_callback(self, callback: Callable):
        """Add a callback for quota violations"""
        self._violation_callbacks.append(callback)
    
    def check_operation(self, operation: str, size: int = 0):
        """Check if an operation would exceed quotas"""
        with self._lock:
            if operation == 'memory_allocate':
                if (self.quota.max_memory and 
                    self.usage.memory_bytes + size > self.quota.max_memory):
                    raise QuotaExceeded(
                        f"Memory allocation of {size} bytes would exceed limit"
                    )
            
            elif operation == 'file_open':
                if (self.quota.max_open_files and 
                    self.usage.file_count + 1 > self.quota.max_open_files):
                    raise QuotaExceeded("Maximum open files limit reached")
            
            elif operation == 'file_write':
                if (self.quota.max_file_size and size > self.quota.max_file_size):
                    raise QuotaExceeded(
                        f"File size {size} exceeds maximum {self.quota.max_file_size}"
                    )
            
            elif operation == 'network_connect':
                if (self.quota.max_network_connections and 
                    self.usage.network_connections + 1 > 
                    self.quota.max_network_connections):
                    raise QuotaExceeded("Maximum network connections limit reached")
            
            elif operation == 'network_transfer':
                if (self.quota.max_data_transfer and 
                    self.usage.network_bytes + size > self.quota.max_data_transfer):
                    raise QuotaExceeded("Maximum data transfer limit exceeded")
    
    def get_usage_report(self) -> Dict[str, Any]:
        """Get current resource usage report"""
        with self._lock:
            return {
                'memory': {
                    'current': self.usage.memory_bytes,
                    'limit': self.quota.max_memory,
                    'percentage': (self.usage.memory_bytes / self.quota.max_memory * 100
                                 if self.quota.max_memory else None)
                },
                'cpu': {
                    'time': self.usage.cpu_time,
                    'time_limit': self.quota.max_cpu_time,
                    'percent': self.usage.cpu_percent,
                    'percent_limit': self.quota.max_cpu_percent
                },
                'files': {
                    'open': self.usage.file_count,
                    'limit': self.quota.max_open_files
                },
                'network': {
                    'connections': self.usage.network_connections,
                    'connections_limit': self.quota.max_network_connections,
                    'bytes_transferred': self.usage.network_bytes,
                    'transfer_limit': self.quota.max_data_transfer
                },
                'execution_time': self.usage.execution_time,
                'execution_limit': self.quota.max_execution_time
            }


# Convenience classes for specific limits
class MemoryLimit:
    """Memory limit enforcer"""
    
    def __init__(self, max_bytes: int):
        self.max_bytes = max_bytes
        self.initial_usage = self._get_memory_usage()
    
    def _get_memory_usage(self) -> int:
        """Get current memory usage"""
        return psutil.Process().memory_info().rss
    
    def check(self):
        """Check if memory limit is exceeded"""
        current = self._get_memory_usage()
        if current - self.initial_usage > self.max_bytes:
            raise QuotaExceeded(f"Memory limit exceeded: {current} bytes")
    
    @contextmanager
    def enforce(self):
        """Context manager to enforce memory limit"""
        try:
            yield
        finally:
            self.check()


class CPULimit:
    """CPU time limit enforcer"""
    
    def __init__(self, max_seconds: float):
        self.max_seconds = max_seconds
        self.start_time = time.time()
    
    def check(self):
        """Check if CPU time limit is exceeded"""
        elapsed = time.time() - self.start_time
        if elapsed > self.max_seconds:
            raise QuotaExceeded(f"CPU time limit exceeded: {elapsed}s")
    
    @contextmanager
    def enforce(self):
        """Context manager to enforce CPU limit"""
        try:
            yield
        finally:
            self.check()


class NetworkLimit:
    """Network usage limit enforcer"""
    
    def __init__(self, max_bytes: int):
        self.max_bytes = max_bytes
        self.bytes_transferred = 0
        self._lock = threading.Lock()
    
    def add_transfer(self, bytes_count: int):
        """Record a data transfer"""
        with self._lock:
            self.bytes_transferred += bytes_count
            if self.bytes_transferred > self.max_bytes:
                raise QuotaExceeded(
                    f"Network transfer limit exceeded: {self.bytes_transferred} bytes"
                )
    
    def get_remaining(self) -> int:
        """Get remaining transfer allowance"""
        with self._lock:
            return max(0, self.max_bytes - self.bytes_transferred)


# Global resource monitor for current context
_current_monitor: Optional[ResourceMonitor] = None


def set_resource_limits(quota: ResourceQuota) -> ResourceMonitor:
    """Set resource limits for current execution"""
    global _current_monitor
    _current_monitor = ResourceMonitor(quota)
    _current_monitor.start_monitoring()
    return _current_monitor


def get_current_monitor() -> Optional[ResourceMonitor]:
    """Get current resource monitor"""
    return _current_monitor


def check_resource_operation(operation: str, size: int = 0):
    """Check if a resource operation is allowed"""
    if _current_monitor:
        _current_monitor.check_operation(operation, size)


@contextmanager
def with_resource_limits(quota: ResourceQuota):
    """Execute code with resource limits"""
    monitor = ResourceMonitor(quota)
    monitor.start_monitoring()
    
    global _current_monitor
    old_monitor = _current_monitor
    _current_monitor = monitor
    
    try:
        yield monitor
    finally:
        monitor.stop_monitoring()
        _current_monitor = old_monitor