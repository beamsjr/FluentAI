//! Real-time visualization for FluentAi AST and VM execution

pub mod debug;
pub mod layout;
pub mod serializer;
pub mod server;

pub use debug::{DebugEvent, DebugEventReceiver, DebugEventSender};
pub use server::{VisualizationServer, ServerConfig, ServerHandle};