# Network Effects Implementation Guide

## Overview

The Network Effects implementation in FluentAI provides comprehensive networking capabilities including HTTP client/server functionality, WebSocket support, and high-level patterns for REST APIs and OAuth2 authentication. This guide covers the complete implementation across multiple phases.

## Architecture

### Core Components

1. **Effect Handlers**
   - `NetworkHandler` - HTTP client operations
   - `HttpServerHandler` - HTTP server with routing
   - `WebSocketHandler` - WebSocket connections

2. **Standard Library Modules**
   - `http.flc` - Basic HTTP operations
   - `rest.flc` - Advanced REST client
   - `oauth2.flc` - OAuth2 authentication

3. **Supporting Infrastructure**
   - `FluentRouter` - Dynamic routing with path parameters
   - `EffectContext` - Effect handler registration
   - Request/Response types

## Phase 1: HTTP Client (Completed)

### Implementation Details

The HTTP client is implemented in `fluentai-effects/src/handlers/network.rs`:

```rust
pub struct NetworkHandler {
    client: Client,
}
```

### Supported Operations

- `request` - Make HTTP request with full control
- `get`, `post`, `put`, `delete`, `patch`, `head`, `options` - Convenience methods

### Features

- Async/await support
- Custom headers
- Request/response bodies
- Timeout configuration
- Error handling

### Usage Example

```fluentai
// Direct Network effect
let response = perform Network.request({
    "method": "POST",
    "url": "https://api.example.com/users",
    "headers": {"Authorization": "Bearer token"},
    "body": {"name": "Alice"}.to_json(),
    "timeout": 5000
}).await();

// Using http module
use http;
let response = http.post(
    "https://api.example.com/users",
    {"name": "Alice"}.to_json(),
    {"Authorization": "Bearer token"}
).await();
```

## Phase 2: HTTP Server & WebSocket (Completed)

### HTTP Server Implementation

The HTTP server is implemented in `fluentai-effects/src/handlers/http_server.rs`:

```rust
pub struct HttpServerHandler {
    servers: Arc<RwLock<FxHashMap<String, ServerInfo>>>,
    route_registry: Arc<RwLock<FxHashMap<String, RouteInfo>>>,
    websocket_handlers: Arc<RwLock<FxHashMap<String, Arc<WebSocketHandler>>>>,
}
```

### Routing System

Dynamic routing with path parameters is handled by `fluentai-effects/src/router.rs`:

```rust
pub struct Router {
    routes: Vec<Route>,
}

pub struct Route {
    method: String,
    segments: Vec<RouteSegment>,
    handler_id: String,
}
```

Features:
- Path parameters: `/api/users/:id`
- Query string parsing
- Method-based routing
- Catch-all routes

### WebSocket Support

WebSocket functionality is implemented in `fluentai-effects/src/handlers/websocket.rs`:

```rust
pub struct WebSocketHandler {
    connections: Arc<RwLock<FxHashMap<String, WebSocketConnection>>>,
}
```

Operations:
- `connections` - List active connections
- `send` - Send to specific connection
- `broadcast` - Send to all connections
- `close` - Close connection

### Server Example

```fluentai
use http;

// Define route handlers
private async function handle_users(ctx: RouteContext) -> RouteResponse {
    let users = get_users_from_db();
    http.json_response(users)
}

private async function handle_user(ctx: RouteContext) -> RouteResponse {
    let user_id = ctx.params["id"];
    let user = get_user_by_id(user_id);
    http.json_response(user)
}

// Start server with routes
let server = http.serve(8080, [
    http.route_get("/api/users", "handle_users"),
    http.route_get("/api/users/:id", "handle_user"),
    http.route_post("/api/users", "handle_create_user")
]).await();

// Enable WebSocket
let ws_server = perform HttpServer.listen(8081, true).await();

// Handle WebSocket connections
perform HttpServer.route("GET", "/ws", "handle_websocket");
```

## Phase 3: HTTP Patterns (Completed)

### REST Client Module

The `rest.flc` module provides:

1. **RestClient** - Configurable HTTP client
   - Retry logic with exponential backoff
   - Timeout handling
   - Header management
   - Authentication helpers

2. **RestResponse** - Enhanced response handling
   - Success/error status
   - JSON parsing
   - Duration tracking

3. **CRUD Client** - Type-safe resource operations
   - List, Get, Create, Update, Patch, Delete
   - Consistent API patterns

### OAuth2 Module

The `oauth2.flc` module implements:

1. **OAuth2 Flows**
   - Authorization Code
   - Client Credentials
   - Refresh Token

2. **Token Management**
   - Automatic refresh
   - Expiration checking
   - Storage interfaces

3. **Provider Presets**
   - GitHub
   - Google
   - Microsoft

### Pattern Examples

#### REST Client with Retry

```fluentai
use rest;

let client = rest.client("https://api.example.com")
    .with_auth("api-key")
    .with_retry(rest.RetryConfig {
        max_retries: 3,
        initial_delay: 100,
        max_delay: 5000,
        exponential_base: 2.0
    });

let data = client.get("/data").await().json();
```

#### OAuth2 Authentication

```fluentai
use oauth2;

let github = oauth2.github_oauth2(
    "client-id",
    "client-secret",
    "http://localhost:8080/callback"
);

let oauth_client = oauth2.oauth2_client(github);
let auth_url = oauth_client.auth_url();

// After user authorizes...
let token = oauth_client.exchange_code(auth_code).await();
let api = oauth_client.rest_client("https://api.github.com").await();
```

## Testing

### Unit Tests

Located in `fluentai-effects/tests/`:
- `network_test.rs` - HTTP client tests
- `http_server_test.rs` - Server and routing tests
- `websocket_test.rs` - WebSocket tests
- `rest_client_test.rs` - REST patterns tests

Run tests:
```bash
cargo test -p fluentai-effects
```

### Integration Examples

Complete examples in `/examples/`:
- `rest_client_example.flc` - REST client usage
- `github_api_client.flc` - OAuth2 integration
- `websocket_chat.flc` - Chat server
- `websocket_echo.flc` - Echo server

## Performance Considerations

1. **Connection Pooling**: Automatic via reqwest
2. **Async I/O**: All operations non-blocking
3. **Channel Buffers**: 100-message default for WebSocket
4. **Retry Delays**: Exponential backoff prevents overload

## Security Considerations

1. **HTTPS by Default**: HTTP URLs upgraded to HTTPS
2. **Token Storage**: Use secure storage in production
3. **CORS**: Enabled with permissive policy (configure for production)
4. **Rate Limiting**: Implement at application level

## Future Enhancements

1. **VM Integration**: Execute route handlers in VM
2. **HTTP/2 Support**: Already supported by underlying libraries
3. **gRPC Support**: Could be added as new effect type
4. **Connection Pooling Config**: Expose configuration options
5. **Metrics & Monitoring**: Add instrumentation

## Migration Guide

### From Basic HTTP to REST Client

Before:
```fluentai
let response = perform Network.request({
    "method": "GET",
    "url": "https://api.example.com/users",
    "headers": {"Authorization": "Bearer token"}
}).await();

let users = response.body.from_json();
```

After:
```fluentai
use rest;

let client = rest.client("https://api.example.com")
    .with_auth("token");

let users = client.get("/users").await().json();
```

### Adding OAuth2 to Existing Client

```fluentai
use oauth2;
use rest;

// Configure OAuth2
let oauth = oauth2.oauth2_client(config);

// Get authenticated REST client
let client = oauth.rest_client("https://api.example.com").await();

// Use as normal
let data = client.get("/protected").await().json();
```

## Troubleshooting

### Common Issues

1. **Timeout Errors**
   - Increase timeout: `.with_timeout(30000)`
   - Check network connectivity
   - Verify server responsiveness

2. **OAuth2 Token Refresh**
   - Ensure refresh_token is stored
   - Check token expiration logic
   - Verify client credentials

3. **WebSocket Disconnections**
   - Implement reconnection logic
   - Check for network interruptions
   - Monitor server load

4. **Route Not Found**
   - Verify route registration order
   - Check path parameter syntax
   - Enable debug logging

### Debug Logging

Enable tracing for detailed logs:
```rust
tracing::info!("HTTP request: {} {}", method, url);
```

## Conclusion

The Network Effects implementation provides a complete, production-ready networking stack for FluentAI applications. From basic HTTP requests to complex OAuth2 flows and real-time WebSocket communication, developers have access to powerful, ergonomic APIs that integrate seamlessly with the FluentAI effect system.