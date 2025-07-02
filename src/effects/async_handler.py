"""
Async Effect Handler for ClaudeLang

This module implements asynchronous execution support with promises,
async/await, and concurrent execution primitives.
"""

import asyncio
import threading
from typing import Dict, Any, List, Optional, Callable, Union, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto
from queue import Queue
import time
import uuid

from ..core.ast import EffectType
from .handlers import EffectHandler, EffectRequest, EffectResult


class PromiseState(Enum):
    """States of a promise"""
    PENDING = auto()
    FULFILLED = auto()
    REJECTED = auto()


@dataclass
class Promise:
    """Promise/Future implementation"""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    state: PromiseState = PromiseState.PENDING
    value: Any = None
    error: Any = None
    callbacks: List[Tuple[Callable, Callable]] = field(default_factory=list)
    
    def resolve(self, value: Any):
        """Resolve the promise with a value"""
        if self.state != PromiseState.PENDING:
            return
        
        self.state = PromiseState.FULFILLED
        self.value = value
        
        # Execute callbacks
        for on_fulfilled, _ in self.callbacks:
            if on_fulfilled:
                on_fulfilled(value)
    
    def reject(self, error: Any):
        """Reject the promise with an error"""
        if self.state != PromiseState.PENDING:
            return
        
        self.state = PromiseState.REJECTED
        self.error = error
        
        # Execute callbacks
        for _, on_rejected in self.callbacks:
            if on_rejected:
                on_rejected(error)
    
    def then(self, on_fulfilled: Optional[Callable] = None, 
             on_rejected: Optional[Callable] = None) -> 'Promise':
        """Chain promise handlers"""
        new_promise = Promise()
        
        def handle_fulfilled(value):
            try:
                if on_fulfilled:
                    result = on_fulfilled(value)
                    if isinstance(result, Promise):
                        # Chain promises
                        result.then(new_promise.resolve, new_promise.reject)
                    else:
                        new_promise.resolve(result)
                else:
                    new_promise.resolve(value)
            except Exception as e:
                new_promise.reject(e)
        
        def handle_rejected(error):
            try:
                if on_rejected:
                    result = on_rejected(error)
                    if isinstance(result, Promise):
                        result.then(new_promise.resolve, new_promise.reject)
                    else:
                        new_promise.resolve(result)
                else:
                    new_promise.reject(error)
            except Exception as e:
                new_promise.reject(e)
        
        if self.state == PromiseState.FULFILLED:
            handle_fulfilled(self.value)
        elif self.state == PromiseState.REJECTED:
            handle_rejected(self.error)
        else:
            self.callbacks.append((handle_fulfilled, handle_rejected))
        
        return new_promise


@dataclass
class AsyncTask:
    """Represents an async task being executed"""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    promise: Promise = field(default_factory=Promise)
    coroutine: Optional[Any] = None
    thread: Optional[threading.Thread] = None
    cancelled: bool = False


class AsyncHandler(EffectHandler):
    """Handler for async effects"""
    
    def __init__(self, event_loop: Optional[asyncio.AbstractEventLoop] = None):
        super().__init__()
        self.promises: Dict[str, Promise] = {}
        self.tasks: Dict[str, AsyncTask] = {}
        self.event_loop = event_loop
        self._executor_thread: Optional[threading.Thread] = None
        self._task_queue: Queue = Queue()
        self._running = True
        
        # Start async executor if no event loop provided
        if not self.event_loop:
            self._start_executor()
    
    def _start_executor(self):
        """Start the async executor thread"""
        def run_loop():
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            self.event_loop = loop
            
            async def process_tasks():
                while self._running:
                    # Check for new tasks
                    while not self._task_queue.empty():
                        task = self._task_queue.get()
                        if asyncio.iscoroutine(task):
                            asyncio.create_task(task)
                    
                    # Small delay to prevent busy waiting
                    await asyncio.sleep(0.01)
            
            loop.run_until_complete(process_tasks())
            loop.close()
        
        self._executor_thread = threading.Thread(target=run_loop, daemon=True)
        self._executor_thread.start()
        
        # Wait for loop to be ready
        while not self.event_loop:
            time.sleep(0.01)
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.ASYNC and not operation.startswith("concurrent:")
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "promise":
            # Create a new promise
            promise = Promise()
            self.promises[promise.id] = promise
            
            # If executor function provided, run it
            if args and callable(args[0]):
                executor = args[0]
                
                def run_executor():
                    try:
                        # For simple cases, check if it's a Lambda data
                        if hasattr(executor, 'data') and callable(executor.data):
                            executor.data(promise.resolve, promise.reject)
                        else:
                            executor(promise.resolve, promise.reject)
                    except Exception as e:
                        promise.reject(e)
                
                # Run executor in thread
                thread = threading.Thread(target=run_executor, daemon=True)
                thread.start()
            
            return EffectResult(value=promise.id)
        
        elif op == "resolve":
            # Resolve a promise
            promise_id, value = args[0], args[1] if len(args) > 1 else None
            if promise_id in self.promises:
                self.promises[promise_id].resolve(value)
            return EffectResult(value=None)
        
        elif op == "reject":
            # Reject a promise
            promise_id, error = args[0], args[1] if len(args) > 1 else None
            if promise_id in self.promises:
                self.promises[promise_id].reject(error)
            return EffectResult(value=None)
        
        elif op == "await":
            # Await a promise
            promise_id = args[0]
            if promise_id not in self.promises:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-promise", f"Unknown promise: {promise_id}"])
                    ]
                )
            
            promise = self.promises[promise_id]
            
            # If already resolved, return immediately
            if promise.state == PromiseState.FULFILLED:
                return EffectResult(value=promise.value)
            elif promise.state == PromiseState.REJECTED:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["promise-rejected", promise.error])
                    ]
                )
            
            # Otherwise, block until resolved
            # In a real implementation, this would yield control
            # For now, we'll use a simple polling approach
            timeout = args[1] if len(args) > 1 else None
            start_time = time.time()
            max_wait = 5.0  # Maximum wait time to prevent infinite loops
            
            while promise.state == PromiseState.PENDING:
                if timeout and (time.time() - start_time) > timeout:
                    return EffectResult(
                        value=None,
                        secondary_effects=[
                            EffectRequest(EffectType.ERROR, "raise", 
                                        ["timeout", f"Promise {promise_id} timed out"])
                        ]
                    )
                if (time.time() - start_time) > max_wait:
                    return EffectResult(
                        value=None,
                        secondary_effects=[
                            EffectRequest(EffectType.ERROR, "raise", 
                                        ["timeout", f"Promise {promise_id} exceeded max wait time"])
                        ]
                    )
                time.sleep(0.001)  # Small delay
            
            if promise.state == PromiseState.FULFILLED:
                return EffectResult(value=promise.value)
            else:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["promise-rejected", promise.error])
                    ]
                )
        
        elif op == "then":
            # Chain promise handlers
            promise_id = args[0]
            on_fulfilled = args[1] if len(args) > 1 else None
            on_rejected = args[2] if len(args) > 2 else None
            
            if promise_id not in self.promises:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["invalid-promise", f"Unknown promise: {promise_id}"])
                    ]
                )
            
            promise = self.promises[promise_id]
            new_promise = promise.then(on_fulfilled, on_rejected)
            self.promises[new_promise.id] = new_promise
            
            return EffectResult(value=new_promise.id)
        
        elif op == "all":
            # Wait for all promises
            promise_ids = args[0]
            promises = []
            
            for pid in promise_ids:
                if pid in self.promises:
                    promises.append(self.promises[pid])
                else:
                    return EffectResult(
                        value=None,
                        secondary_effects=[
                            EffectRequest(EffectType.ERROR, "raise", 
                                        ["invalid-promise", f"Unknown promise: {pid}"])
                        ]
                    )
            
            # Create new promise that resolves when all complete
            all_promise = Promise()
            self.promises[all_promise.id] = all_promise
            
            results = []
            completed = 0
            
            def check_completion():
                nonlocal completed
                completed += 1
                if completed == len(promises):
                    all_promise.resolve(results)
            
            for i, promise in enumerate(promises):
                idx = i  # Capture index
                
                def on_fulfilled(value):
                    results.insert(idx, value)
                    check_completion()
                
                def on_rejected(error):
                    all_promise.reject(error)
                
                if promise.state == PromiseState.FULFILLED:
                    results.insert(idx, promise.value)
                    completed += 1
                elif promise.state == PromiseState.REJECTED:
                    all_promise.reject(promise.error)
                    break
                else:
                    promise.then(on_fulfilled, on_rejected)
            
            if completed == len(promises):
                all_promise.resolve(results)
            
            return EffectResult(value=all_promise.id)
        
        elif op == "race":
            # Race promises - first to complete wins
            promise_ids = args[0]
            promises = []
            
            for pid in promise_ids:
                if pid in self.promises:
                    promises.append(self.promises[pid])
            
            race_promise = Promise()
            self.promises[race_promise.id] = race_promise
            
            for promise in promises:
                if promise.state == PromiseState.FULFILLED:
                    race_promise.resolve(promise.value)
                    break
                elif promise.state == PromiseState.REJECTED:
                    race_promise.reject(promise.error)
                    break
                else:
                    promise.then(race_promise.resolve, race_promise.reject)
            
            return EffectResult(value=race_promise.id)
        
        elif op == "delay":
            # Create a promise that resolves after delay
            delay_ms = args[0]
            value = args[1] if len(args) > 1 else None
            
            promise = Promise()
            self.promises[promise.id] = promise
            
            def delayed_resolve():
                time.sleep(delay_ms / 1000.0)
                promise.resolve(value)
            
            thread = threading.Thread(target=delayed_resolve, daemon=True)
            thread.start()
            
            return EffectResult(value=promise.id)
        
        elif op == "async-call":
            # Call an async function
            async_fn = args[0]
            fn_args = args[1:] if len(args) > 1 else []
            
            promise = Promise()
            self.promises[promise.id] = promise
            
            async def run_async():
                try:
                    result = await async_fn(*fn_args)
                    promise.resolve(result)
                except Exception as e:
                    promise.reject(e)
            
            # Schedule coroutine
            if self.event_loop:
                self._task_queue.put(run_async())
            
            return EffectResult(value=promise.id)
        
        elif op == "get-state":
            # Get promise state
            promise_id = args[0]
            if promise_id in self.promises:
                promise = self.promises[promise_id]
                return EffectResult(value={
                    "state": promise.state.name.lower(),
                    "value": promise.value if promise.state == PromiseState.FULFILLED else None,
                    "error": promise.error if promise.state == PromiseState.REJECTED else None
                })
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown async operation: {op}")
    
    def cleanup(self):
        """Clean up resources"""
        self._running = False
        
        # Cancel all pending tasks
        for task in self.tasks.values():
            task.cancelled = True
        
        # Clear promises
        self.promises.clear()
        self.tasks.clear()
        
        # Stop executor thread
        if self._executor_thread:
            self._executor_thread.join(timeout=1.0)