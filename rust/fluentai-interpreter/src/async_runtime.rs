//! Async runtime support for the interpreter

use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};
use tokio::runtime::Runtime;
use tokio::sync::oneshot;

use crate::error::{InterpreterError, InterpreterResult};
use crate::value::Value;

/// Async task handle
pub struct AsyncHandle {
    receiver: oneshot::Receiver<InterpreterResult<Value>>,
    task_id: usize,
}

impl Future for AsyncHandle {
    type Output = InterpreterResult<Value>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::new(&mut self.receiver).poll(cx) {
            Poll::Ready(Ok(result)) => Poll::Ready(result),
            Poll::Ready(Err(_)) => Poll::Ready(Err(InterpreterError::AsyncError(
                "Task cancelled".to_string(),
            ))),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// Async runtime for the interpreter
pub struct AsyncRuntime {
    runtime: Runtime,
    next_task_id: Arc<Mutex<usize>>,
}

impl AsyncRuntime {
    /// Create a new async runtime
    pub fn new() -> InterpreterResult<Self> {
        let runtime = Runtime::new().map_err(|e| {
            InterpreterError::AsyncError(format!("Failed to create runtime: {}", e))
        })?;

        Ok(Self {
            runtime,
            next_task_id: Arc::new(Mutex::new(0)),
        })
    }

    /// Spawn an async task
    pub fn spawn<F>(&self, future: F) -> AsyncHandle
    where
        F: Future<Output = InterpreterResult<Value>> + Send + 'static,
        F::Output: Send,
    {
        let (sender, receiver) = oneshot::channel();
        let task_id = {
            let mut id = self.next_task_id.lock().unwrap();
            let current = *id;
            *id += 1;
            current
        };

        self.runtime.spawn(async move {
            let result = future.await;
            let _ = sender.send(result);
        });

        AsyncHandle { receiver, task_id }
    }

    /// Block on a future
    pub fn block_on<F>(&self, future: F) -> InterpreterResult<Value>
    where
        F: Future<Output = InterpreterResult<Value>>,
    {
        self.runtime.block_on(future)
    }

    /// Create a channel for async communication
    pub fn create_channel() -> (AsyncSender, AsyncReceiver) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        (AsyncSender { sender: tx }, AsyncReceiver { receiver: rx })
    }
}

/// Async channel sender
pub struct AsyncSender {
    sender: tokio::sync::mpsc::UnboundedSender<Value>,
}

impl AsyncSender {
    /// Send a value
    pub fn send(&self, value: Value) -> InterpreterResult<()> {
        self.sender
            .send(value)
            .map_err(|_| InterpreterError::AsyncError("Channel closed".to_string()))
    }
}

/// Async channel receiver
pub struct AsyncReceiver {
    receiver: tokio::sync::mpsc::UnboundedReceiver<Value>,
}

impl AsyncReceiver {
    /// Receive a value
    pub async fn recv(&mut self) -> InterpreterResult<Value> {
        self.receiver
            .recv()
            .await
            .ok_or_else(|| InterpreterError::AsyncError("Channel closed".to_string()))
    }
}

impl Default for AsyncRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create async runtime")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_runtime() {
        let runtime = AsyncRuntime::new().unwrap();

        let result = runtime.block_on(async { Ok(Value::from_integer(42)) });

        assert_eq!(result.unwrap().to_integer(), Some(42));
    }

    // TODO: Fix this test - Value is not Send due to Rc<Closure>
    // #[test]
    // fn test_async_spawn() {
    //     use std::sync::Arc;
    //
    //     let runtime = AsyncRuntime::new().unwrap();
    //
    //     // Test with a simple Send-safe type
    //     let value = Arc::new(42i64);
    //     let value_clone = value.clone();
    //
    //     let handle = runtime.spawn(async move {
    //         // Create the Value inside the spawned task
    //         Ok(Value::from_integer(*value_clone))
    //     });
    //
    //     let result = runtime.block_on(handle);
    //     assert_eq!(result.unwrap().to_integer(), Some(42));
    // }
}
