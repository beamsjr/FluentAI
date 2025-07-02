"""
Concurrency Effect Handler for ClaudeLang

This module implements concurrency primitives including channels,
goroutines (lightweight threads), select statements, and synchronization.
"""

import threading
import queue as queue_module
import time
import uuid
from typing import Dict, Any, List, Optional, Callable, Union, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto
import concurrent.futures

from ..core.ast import EffectType
from .handlers import EffectHandler, EffectRequest, EffectResult


class ChannelState(Enum):
    """States of a channel"""
    OPEN = auto()
    CLOSED = auto()


@dataclass
class Channel:
    """Channel for communication between concurrent tasks"""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    capacity: int = 0  # 0 = unbuffered
    queue: queue_module.Queue = field(init=False)
    state: ChannelState = ChannelState.OPEN
    readers_waiting: int = 0
    writers_waiting: int = 0
    
    def __post_init__(self):
        self.queue = queue_module.Queue(maxsize=self.capacity)
    
    def send(self, value: Any, timeout: Optional[float] = None) -> bool:
        """Send a value to the channel"""
        if self.state == ChannelState.CLOSED:
            raise ValueError("Cannot send to closed channel")
        
        try:
            self.queue.put(value, timeout=timeout)
            return True
        except queue_module.Full:
            return False
    
    def receive(self, timeout: Optional[float] = None) -> Tuple[Any, bool]:
        """Receive a value from the channel"""
        if self.state == ChannelState.CLOSED and self.queue.empty():
            return None, False
        
        try:
            value = self.queue.get(timeout=timeout)
            return value, True
        except queue_module.Empty:
            return None, False
    
    def close(self):
        """Close the channel"""
        self.state = ChannelState.CLOSED
    
    def is_closed(self) -> bool:
        """Check if channel is closed"""
        return self.state == ChannelState.CLOSED


@dataclass
class Goroutine:
    """Lightweight thread for concurrent execution"""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    thread: Optional[threading.Thread] = None
    future: Optional[concurrent.futures.Future] = None
    result: Any = None
    error: Optional[Exception] = None
    done: bool = False


class ConcurrencyHandler(EffectHandler):
    """Handler for concurrency effects"""
    
    def __init__(self, max_workers: int = 10):
        super().__init__()
        self.channels: Dict[str, Channel] = {}
        self.goroutines: Dict[str, Goroutine] = {}
        self.executor = concurrent.futures.ThreadPoolExecutor(max_workers=max_workers)
        self.locks: Dict[str, threading.Lock] = {}
        self.conditions: Dict[str, threading.Condition] = {}
        self.semaphores: Dict[str, threading.Semaphore] = {}
        self.barriers: Dict[str, threading.Barrier] = {}
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.ASYNC and operation.startswith("concurrent:")
    
    def handle(self, request: EffectRequest) -> EffectResult:
        # Remove the "concurrent:" prefix
        op = request.operation.split(":", 1)[1] if ":" in request.operation else request.operation
        args = request.arguments
        
        if op == "channel":
            # Create a channel
            capacity = args[0] if args else 0
            channel = Channel(capacity=capacity)
            self.channels[channel.id] = channel
            return EffectResult(value=channel.id)
        
        elif op == "send":
            # Send to channel
            channel_id, value = args[0], args[1]
            timeout = args[2] if len(args) > 2 else None
            
            if channel_id not in self.channels:
                return EffectResult(
                    value=False,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-channel", f"Unknown channel: {channel_id}"])
                    ]
                )
            
            channel = self.channels[channel_id]
            try:
                success = channel.send(value, timeout)
                return EffectResult(value=success)
            except ValueError as e:
                return EffectResult(
                    value=False,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["channel-closed", str(e)])
                    ]
                )
        
        elif op == "receive":
            # Receive from channel
            channel_id = args[0]
            timeout = args[1] if len(args) > 1 else None
            
            if channel_id not in self.channels:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-channel", f"Unknown channel: {channel_id}"])
                    ]
                )
            
            channel = self.channels[channel_id]
            value, ok = channel.receive(timeout)
            
            return EffectResult(value={"value": value, "ok": ok})
        
        elif op == "close":
            # Close channel
            channel_id = args[0]
            
            if channel_id in self.channels:
                self.channels[channel_id].close()
            
            return EffectResult(value=None)
        
        elif op == "go":
            # Launch goroutine
            func = args[0]
            func_args = args[1:] if len(args) > 1 else []
            
            goroutine = Goroutine()
            self.goroutines[goroutine.id] = goroutine
            
            def run_goroutine():
                try:
                    # Execute the function
                    result = func(*func_args) if callable(func) else None
                    goroutine.result = result
                    goroutine.done = True
                except Exception as e:
                    goroutine.error = e
                    goroutine.done = True
            
            # Submit to thread pool
            future = self.executor.submit(run_goroutine)
            goroutine.future = future
            
            return EffectResult(value=goroutine.id)
        
        elif op == "join":
            # Wait for goroutine to complete
            goroutine_id = args[0]
            timeout = args[1] if len(args) > 1 else None
            
            if goroutine_id not in self.goroutines:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-goroutine", f"Unknown goroutine: {goroutine_id}"])
                    ]
                )
            
            goroutine = self.goroutines[goroutine_id]
            
            # Wait for completion
            if goroutine.future:
                try:
                    goroutine.future.result(timeout=timeout)
                except concurrent.futures.TimeoutError:
                    return EffectResult(
                        value=None,
                        secondary_effects=[
                            EffectRequest(EffectType.ERROR, "raise", 
                                        ["timeout", "Goroutine join timed out"])
                        ]
                    )
            
            if goroutine.error:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["goroutine-error", str(goroutine.error)])
                    ]
                )
            
            return EffectResult(value=goroutine.result)
        
        elif op == "select":
            # Select from multiple channels
            cases = args[0]  # List of {"channel": id, "op": "send"/"receive", "value": val}
            default = args[1] if len(args) > 1 else None
            timeout = args[2] if len(args) > 2 else 0.0
            
            # Try each case
            start_time = time.time()
            while True:
                for i, case in enumerate(cases):
                    channel_id = case["channel"]
                    operation = case["op"]
                    
                    if channel_id not in self.channels:
                        continue
                    
                    channel = self.channels[channel_id]
                    
                    if operation == "receive":
                        try:
                            value = channel.queue.get_nowait()
                            return EffectResult(value={
                                "index": i,
                                "value": value,
                                "ok": True
                            })
                        except queue_module.Empty:
                            pass
                    
                    elif operation == "send":
                        value = case.get("value")
                        try:
                            channel.queue.put_nowait(value)
                            return EffectResult(value={
                                "index": i,
                                "value": None,
                                "ok": True
                            })
                        except queue_module.Full:
                            pass
                
                # Check timeout
                if timeout is not None and (time.time() - start_time) > timeout:
                    if default is not None:
                        return EffectResult(value={
                            "index": -1,
                            "value": default,
                            "ok": False
                        })
                    break
                
                # Small delay to prevent busy waiting
                time.sleep(0.001)
            
            return EffectResult(value={"index": -1, "value": None, "ok": False})
        
        elif op == "mutex":
            # Create mutex
            mutex_id = str(uuid.uuid4())
            self.locks[mutex_id] = threading.Lock()
            return EffectResult(value=mutex_id)
        
        elif op == "lock":
            # Lock mutex
            mutex_id = args[0]
            timeout = args[1] if len(args) > 1 else None
            
            if mutex_id not in self.locks:
                return EffectResult(
                    value=False,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-mutex", f"Unknown mutex: {mutex_id}"])
                    ]
                )
            
            lock = self.locks[mutex_id]
            acquired = lock.acquire(timeout=timeout)
            return EffectResult(value=acquired)
        
        elif op == "unlock":
            # Unlock mutex
            mutex_id = args[0]
            
            if mutex_id not in self.locks:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-mutex", f"Unknown mutex: {mutex_id}"])
                    ]
                )
            
            try:
                self.locks[mutex_id].release()
                return EffectResult(value=True)
            except RuntimeError:
                return EffectResult(value=False)
        
        elif op == "semaphore":
            # Create semaphore
            initial = args[0] if args else 1
            sem_id = str(uuid.uuid4())
            self.semaphores[sem_id] = threading.Semaphore(initial)
            return EffectResult(value=sem_id)
        
        elif op == "acquire":
            # Acquire semaphore
            sem_id = args[0]
            timeout = args[1] if len(args) > 1 else None
            
            if sem_id not in self.semaphores:
                return EffectResult(
                    value=False,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-semaphore", f"Unknown semaphore: {sem_id}"])
                    ]
                )
            
            acquired = self.semaphores[sem_id].acquire(timeout=timeout)
            return EffectResult(value=acquired)
        
        elif op == "release":
            # Release semaphore
            sem_id = args[0]
            n = args[1] if len(args) > 1 else 1
            
            if sem_id not in self.semaphores:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-semaphore", f"Unknown semaphore: {sem_id}"])
                    ]
                )
            
            for _ in range(n):
                self.semaphores[sem_id].release()
            
            return EffectResult(value=True)
        
        elif op == "barrier":
            # Create barrier
            parties = args[0]
            timeout = args[1] if len(args) > 1 else None
            barrier_id = str(uuid.uuid4())
            self.barriers[barrier_id] = threading.Barrier(parties, timeout=timeout)
            return EffectResult(value=barrier_id)
        
        elif op == "wait":
            # Wait at barrier
            barrier_id = args[0]
            timeout = args[1] if len(args) > 1 else None
            
            if barrier_id not in self.barriers:
                return EffectResult(
                    value=-1,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-barrier", f"Unknown barrier: {barrier_id}"])
                    ]
                )
            
            try:
                index = self.barriers[barrier_id].wait(timeout)
                return EffectResult(value=index)
            except threading.BrokenBarrierError:
                return EffectResult(value=-1)
        
        elif op == "worker-pool":
            # Create worker pool
            size = args[0] if args else 4
            pool_id = str(uuid.uuid4())
            # Store pool configuration
            self.state[f"pool_{pool_id}"] = {
                "size": size,
                "tasks": queue_module.Queue(),
                "workers": []
            }
            return EffectResult(value=pool_id)
        
        elif op == "submit":
            # Submit task to worker pool
            pool_id = args[0]
            task = args[1]
            
            pool_key = f"pool_{pool_id}"
            if pool_key not in self.state:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-pool", f"Unknown pool: {pool_id}"])
                    ]
                )
            
            # Add task to queue
            self.state[pool_key]["tasks"].put(task)
            return EffectResult(value=True)
        
        else:
            raise ValueError(f"Unknown concurrency operation: {op}")
    
    def cleanup(self):
        """Clean up resources"""
        # Close all channels
        for channel in self.channels.values():
            channel.close()
        
        # Shutdown executor
        self.executor.shutdown(wait=True)
        
        # Clear all resources
        self.channels.clear()
        self.goroutines.clear()
        self.locks.clear()
        self.conditions.clear()
        self.semaphores.clear()
        self.barriers.clear()