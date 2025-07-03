//! HTTP and WebSocket server for visualization

use axum::{
    extract::{ws::{WebSocket, Message}, WebSocketUpgrade, State},
    response::{Html, IntoResponse},
    routing::{get, get_service},
    Router,
};
use std::{net::SocketAddr, path::PathBuf, sync::Arc};
use tokio::sync::{mpsc, RwLock};
use tower_http::{
    cors::CorsLayer,
    services::ServeDir,
};
use tracing::info;
use futures_util::{SinkExt, StreamExt};

use crate::{
    debug::DebugEventReceiver,
    serializer::{ControlCommand, VisualizationMessage},
};

/// Server configuration
#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub host: String,
    pub port: u16,
    pub static_dir: PathBuf,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 8080,
            static_dir: PathBuf::from("static"),
        }
    }
}

/// Shared server state
#[derive(Clone)]
struct ServerState {
    sessions: Arc<RwLock<Vec<mpsc::UnboundedSender<VisualizationMessage>>>>,
    control_tx: mpsc::UnboundedSender<ControlCommand>,
}

/// Handle to interact with a running visualization server
#[derive(Clone)]
pub struct ServerHandle {
    state: ServerState,
}

impl ServerHandle {
    /// Broadcast a message to all connected clients
    pub async fn broadcast_message(&self, message: VisualizationMessage) {
        let sessions = self.state.sessions.read().await;
        for tx in sessions.iter() {
            let _ = tx.send(message.clone());
        }
    }
}

/// Visualization server
pub struct VisualizationServer {
    config: ServerConfig,
    state: ServerState,
    control_rx: Option<mpsc::UnboundedReceiver<ControlCommand>>,
    debug_rx: Option<DebugEventReceiver>,
}

impl VisualizationServer {
    /// Create a new visualization server
    pub fn new(config: ServerConfig) -> Self {
        let (control_tx, control_rx) = mpsc::unbounded_channel();
        let state = ServerState {
            sessions: Arc::new(RwLock::new(Vec::new())),
            control_tx,
        };
        
        Self {
            config,
            state,
            control_rx: Some(control_rx),
            debug_rx: None,
        }
    }
    
    /// Set the debug event receiver
    pub fn set_debug_receiver(&mut self, receiver: DebugEventReceiver) {
        self.debug_rx = Some(receiver);
    }
    
    /// Get a handle to interact with the server
    pub fn handle(&self) -> ServerHandle {
        ServerHandle {
            state: self.state.clone(),
        }
    }
    
    /// Start the server
    pub async fn run(mut self) -> anyhow::Result<()> {
        let addr = SocketAddr::new(
            self.config.host.parse()?,
            self.config.port,
        );
        
        // Spawn debug event processor if receiver is set
        if let Some(debug_rx) = self.debug_rx.take() {
            let state = self.state.clone();
            tokio::spawn(async move {
                let mut receiver = debug_rx;
                while let Some(event) = receiver.recv().await {
                    let message = VisualizationMessage::DebugEvent { event };
                    // Broadcast to all sessions
                    let sessions = state.sessions.read().await;
                    for tx in sessions.iter() {
                        let _ = tx.send(message.clone());
                    }
                }
            });
        }
        
        // Spawn control command handler if receiver is set
        if let Some(control_rx) = self.control_rx.take() {
            tokio::spawn(async move {
                let mut receiver = control_rx;
                while let Some(command) = receiver.recv().await {
                    // TODO: Handle control commands
                    tracing::debug!("Received control command: {:?}", command);
                }
            });
        }
        
        // Create router
        let app = Router::new()
            .route("/", get(root_handler))
            .route("/ws", get(websocket_handler))
            .nest_service(
                "/static",
                get_service(ServeDir::new(&self.config.static_dir)),
            )
            .layer(CorsLayer::permissive())
            .with_state(self.state.clone());
        
        info!("Visualization server listening on http://{}", addr);
        
        // Start server
        let listener = tokio::net::TcpListener::bind(addr).await?;
        axum::serve(listener, app).await?;
        
        Ok(())
    }
    
    /// Process debug events and broadcast to clients
    pub async fn process_debug_events(&self, mut receiver: DebugEventReceiver) {
        while let Some(event) = receiver.recv().await {
            let message = VisualizationMessage::DebugEvent { event };
            self.broadcast_message(message).await;
        }
    }
    
    /// Broadcast a message to all connected clients
    pub async fn broadcast_message(&self, message: VisualizationMessage) {
        let sessions = self.state.sessions.read().await;
        for tx in sessions.iter() {
            let _ = tx.send(message.clone());
        }
    }
    
    /// Get control command receiver
    pub fn control_receiver(&mut self) -> Option<mpsc::UnboundedReceiver<ControlCommand>> {
        self.control_rx.take()
    }
}

/// Root page handler
async fn root_handler() -> Html<&'static str> {
    Html(include_str!("../static/index.html"))
}

/// WebSocket handler
async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(state): State<ServerState>,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_socket(socket, state))
}

/// Handle a WebSocket connection
async fn handle_socket(socket: WebSocket, state: ServerState) {
    let (tx, mut rx) = mpsc::unbounded_channel();
    
    // Add to sessions
    state.sessions.write().await.push(tx.clone());
    
    // Send connected message
    let session_id = uuid::Uuid::new_v4().to_string();
    let _ = tx.send(VisualizationMessage::Connected { session_id });
    
    // Split socket
    let (mut sender, mut receiver) = socket.split();
    
    // Task to send messages to client
    let tx_task = tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            if let Ok(json) = serde_json::to_string(&msg) {
                if sender.send(Message::Text(json)).await.is_err() {
                    break;
                }
            }
        }
    });
    
    // Task to receive messages from client
    let control_tx = state.control_tx.clone();
    let rx_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = receiver.next().await {
            if let Ok(text) = msg.to_text() {
                if let Ok(viz_msg) = serde_json::from_str::<VisualizationMessage>(text) {
                    if let VisualizationMessage::Control { command } = viz_msg {
                        let _ = control_tx.send(command);
                    }
                }
            }
        }
    });
    
    // Wait for tasks to complete
    let _ = tokio::join!(tx_task, rx_task);
    
    // Remove from sessions
    let mut sessions = state.sessions.write().await;
    sessions.retain(|s| !s.same_channel(&tx));
}

