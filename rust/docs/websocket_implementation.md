# WebSocket Implementation Guide

## Overview

FluentAI now supports WebSocket connections for real-time, bidirectional communication. This implementation is integrated with the HTTP server effect handler, allowing servers to handle both HTTP requests and WebSocket connections.

## Architecture

### Components

1. **WebSocketHandler**: Manages WebSocket connections and message routing
2. **HttpServerHandler**: Extended to support WebSocket upgrades
3. **WebSocket Effect Type**: New effect type for WebSocket operations

### Design Decisions

- **Channel-based Communication**: Each WebSocket connection has a dedicated channel for sending messages
- **Connection Management**: Connections are tracked by unique IDs
- **Integration with HTTP Server**: WebSocket upgrades are handled during HTTP request processing
- **Async Operations**: All network operations are non-blocking

## API Reference

### HTTP Server WebSocket Operations

#### Starting a WebSocket-enabled Server

```fluentai
// Start server with WebSocket support (second parameter = true)
let server = perform HttpServer.listen(8080, true).await();
```

#### WebSocket Operations

##### `ws_connections` (sync)
Get list of active WebSocket connections for a server.

```fluentai
let connections = perform HttpServer.ws_connections("server_8080");
// Returns: List of connection IDs
```

##### `ws_send` (sync)
Send a message to a specific WebSocket connection (synchronous version).

```fluentai
perform HttpServer.ws_send("server_8080", "connection_id", "Hello, client!");
```

##### `ws_send_async` (async)
Send a message to a specific WebSocket connection (asynchronous version).

```fluentai
perform HttpServer.ws_send_async("server_8080", "connection_id", "Hello, client!").await();
```

##### `ws_broadcast` (async)
Broadcast a message to all WebSocket connections on a server.

```fluentai
let sent_count = perform HttpServer.ws_broadcast("server_8080", "Hello, everyone!").await();
// Returns: Number of connections that received the message
```

### WebSocket Handler Operations

The WebSocket handler can be used directly for more fine-grained control:

#### `connections` (sync)
Get list of all active connections.

```fluentai
let connections = perform WebSocket.connections();
```

#### `connection_count` (sync)
Get the number of active connections.

```fluentai
let count = perform WebSocket.connection_count();
```

#### `send` (async)
Send a message to a specific connection.

```fluentai
perform WebSocket.send("connection_id", "message").await();
```

#### `broadcast` (async)
Broadcast a message to all connections.

```fluentai
let sent = perform WebSocket.broadcast("message").await();
```

#### `close` (async)
Close a specific connection.

```fluentai
perform WebSocket.close("connection_id").await();
```

## Usage Examples

### Simple Echo Server

```fluentai
use http;

private async function handle_websocket_message(conn_id: string, message: string) {
    // Echo the message back
    perform HttpServer.ws_send_async("server_8081", conn_id, f"Echo: {message}").await();
}

private async function main() {
    // Start server with WebSocket support
    let server = perform HttpServer.listen(8081, true).await();
    
    $(f"WebSocket echo server running on port {server.port}").print();
    
    // Monitor connections
    loop {
        perform Time.sleep(5000).await();
        let connections = perform HttpServer.ws_connections(server.id);
        $(f"Active connections: {connections.length()}").print();
    }
}
```

### Chat Server

```fluentai
use http;

private let users = {};

private async function handle_new_connection(conn_id: string) {
    users[conn_id] = { "name": f"User_{conn_id.substring(0, 6)}" };
    
    let welcome = {
        "type": "welcome",
        "message": "Welcome to the chat!",
        "user_count": users.keys().length()
    };
    
    perform HttpServer.ws_send_async("server_8080", conn_id, welcome.to_json()).await();
}

private async function handle_chat_message(conn_id: string, message: string) {
    let chat_msg = {
        "type": "chat",
        "from": users[conn_id].name,
        "message": message,
        "timestamp": perform Time.now()
    };
    
    // Broadcast to all users
    perform HttpServer.ws_broadcast("server_8080", chat_msg.to_json()).await();
}

private async function main() {
    let server = perform HttpServer.listen(8080, true).await();
    $(f"Chat server running on port {server.port}").print();
    
    // Main message loop would go here
}
```

## Implementation Details

### Connection Lifecycle

1. **Connection Establishment**:
   - Client requests WebSocket upgrade via HTTP
   - Server validates upgrade request
   - WebSocket handler creates connection entry
   - Unique connection ID is generated

2. **Message Flow**:
   - Incoming messages are received by the WebSocket handler
   - Messages can be routed to specific handlers based on content
   - Outgoing messages are sent through connection channels

3. **Connection Termination**:
   - Client or server initiates close
   - Connection is removed from registry
   - Associated resources are cleaned up

### Current Limitations

1. **Basic Integration**: The current implementation provides basic WebSocket support. Full integration with VM execution for dynamic handlers is pending.

2. **Message Types**: Currently supports text messages. Binary message support exists but needs testing.

3. **Protocol Features**: Advanced WebSocket features like compression, extensions, and subprotocols are not yet implemented.

4. **Error Handling**: Connection errors result in immediate disconnection. More sophisticated error recovery could be added.

## Testing

The implementation includes comprehensive tests covering:

- Connection management
- Message sending and receiving
- Broadcasting
- Connection closure
- Integration with HTTP server

Run tests with:
```bash
cargo test -p fluentai-effects --test websocket_test
```

## Future Enhancements

1. **VM Integration**: Execute user-defined handlers for WebSocket events
2. **Binary Messages**: Full support for binary data transmission
3. **Subprotocols**: Support for WebSocket subprotocols
4. **Connection Metadata**: Store and retrieve custom data per connection
5. **Rate Limiting**: Prevent message flooding
6. **Authentication**: Integrate with auth systems for secure connections
7. **Clustering**: Support for multi-server WebSocket deployments

## Security Considerations

1. **Origin Validation**: Currently accepts all origins. Should add origin checking.
2. **Message Size Limits**: No limits currently enforced. Should add configurable limits.
3. **Authentication**: No built-in auth. Applications must implement their own.
4. **Rate Limiting**: No rate limiting. Applications should implement if needed.

## Performance Considerations

1. **Channel Buffers**: Each connection has a 100-message buffer
2. **Async I/O**: All operations are non-blocking
3. **Connection Limits**: No hard limits, bounded by system resources
4. **Message Broadcasting**: O(n) for n connections

## Migration Guide

To add WebSocket support to an existing HTTP server:

1. Update server creation to enable WebSocket:
   ```fluentai
   // Before
   let server = perform HttpServer.listen(8080).await();
   
   // After
   let server = perform HttpServer.listen(8080, true).await();
   ```

2. Add WebSocket route handlers:
   ```fluentai
   perform HttpServer.route("GET", "/ws", "handle_websocket_upgrade");
   ```

3. Implement message handlers for your application logic

## Conclusion

The WebSocket implementation provides a solid foundation for real-time communication in FluentAI applications. While basic functionality is complete, there are opportunities for enhancement as the platform evolves.