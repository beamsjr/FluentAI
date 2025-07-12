# Network Effects Implementation Guide

## Overview

FluentAI now includes comprehensive HTTP client and server capabilities through the Network Effects system. This document covers the implementation details and usage patterns.

## Components

### 1. HTTP Client (Network Effect)

The `NetworkHandler` provides HTTP client functionality with support for all standard HTTP methods.

#### Available Operations

- `request` - Full HTTP request with method, URL, headers, and body
- `get` - Simple GET request  
- `post` - Simple POST request

#### Example Usage

```fluentai
// Full request with headers
let response = perform Network.request({
    "method": "POST",
    "url": "https://api.example.com/users",
    "headers": {
        "Content-Type": "application/json",
        "Authorization": "Bearer token123"
    },
    "body": data.to_json()
}).await();

// Simple GET request
let data = perform Network.get("https://api.example.com/users").await();
```

### 2. HTTP Server (HttpServer Effect)

The `HttpServerHandler` provides a full-featured HTTP server with dynamic routing.

#### Features

- **Dynamic Path Parameters**: `/api/users/:id`
- **Wildcard Routes**: `/static/*`
- **Query String Parsing**: Automatic extraction of query parameters
- **Method-based Routing**: Support for GET, POST, PUT, DELETE, etc.
- **Async Request Handling**: Non-blocking request processing
- **CORS Support**: Built-in Cross-Origin Resource Sharing

#### Available Operations

##### `route` (sync)
Register a route handler.

```fluentai
perform HttpServer.route("GET", "/api/users/:id", "getUserHandler");
```

##### `listen` (async)
Start the HTTP server on a specified port.

```fluentai
let server = perform HttpServer.listen(8080).await();
// Returns: { id: "server_8080", address: "127.0.0.1:8080", port: 8080 }
```

##### `stop` (async)
Stop a running server.

```fluentai
perform HttpServer.stop(server.id).await();
```

### 3. Router

The routing system supports sophisticated URL pattern matching:

#### Route Patterns

1. **Static Routes**: `/api/users`
2. **Parameter Routes**: `/api/users/:userId/posts/:postId`
3. **Wildcard Routes**: `/static/*`

#### Route Context

Handlers receive a `RouteContext` with:

```fluentai
{
    method: "GET",
    path: "/api/users/123",
    params: { "id": "123" },          // Path parameters
    query: { "page": "2" },           // Query parameters
    headers: { "User-Agent": "..." }, // Request headers
    body: "..."                       // Request body
}
```

### 4. Standard Library Module (http.flc)

The `http` module provides convenient wrappers:

```fluentai
use http;

// Client functions
let response = http.get(url, headers).await();
let response = http.post(url, body, headers).await();

// Server functions
let server = http.serve(8080, [
    http.route_get("/", "homeHandler"),
    http.route_post("/api/users", "createUserHandler")
]).await();

// Response helpers
let json_response = http.json_response(data, 200);
let redirect = http.redirect("/login", 302);
```

## Architecture

### Request Flow

1. Client makes HTTP request to server
2. Axum receives request and passes to fallback handler
3. Router matches URL pattern and extracts parameters
4. Handler task receives route request via channel
5. VM executes handler function (future integration)
6. Response sent back through channel
7. Axum sends HTTP response to client

### Key Design Decisions

1. **Channel-based Communication**: Decouples HTTP server from VM execution
2. **Dynamic Routing**: Supports RESTful API patterns
3. **Async Throughout**: Non-blocking I/O for scalability
4. **Effect System Integration**: Consistent with FluentAI's effect model

## Examples

### Basic HTTP Server

```fluentai
use http;

private function handle_home(ctx: Map) -> Map {
    http.html_response("<h1>Welcome!</h1>")
}

private function handle_api_user(ctx: Map) -> Map {
    let user_id = ctx.params.id;
    let user = get_user(user_id);
    http.json_response(user)
}

private async function main() {
    let server = http.serve(8080, [
        http.route_get("/", "handle_home"),
        http.route_get("/api/users/:id", "handle_api_user")
    ]).await();
    
    $(f"Server running at http://localhost:{server.port}").print();
}
```

### REST API Client

```fluentai
use http;

private async function create_user(name: string, email: string) {
    let user_data = { "name": name, "email": email };
    
    let response = http.post(
        "https://api.example.com/users",
        user_data.to_json(),
        { "Content-Type": "application/json" }
    ).await();
    
    if response.status == http.CREATED {
        let created_user = http.parse_json(response);
        $(f"Created user: {created_user.id}").print();
    } else {
        $(f"Error: {response.status}").print();
    }
}
```

## Future Enhancements

### Phase 2: WebSocket Support (net-6)
- Upgrade HTTP connections to WebSocket
- Bidirectional messaging
- Real-time communication patterns

### Phase 3: Advanced Patterns (net-8)
- OAuth2 authentication flow
- Request/response interceptors
- Connection pooling configuration
- HTTP/2 support

## Testing

The implementation includes comprehensive tests:

- Router pattern matching tests
- HTTP server lifecycle tests  
- Route registration tests
- Error handling tests

Run tests with:
```bash
cargo test -p fluentai-effects --test http_server_test
```

## Performance Considerations

1. **Connection Reuse**: The HTTP client uses connection pooling
2. **Async I/O**: Non-blocking operations throughout
3. **Efficient Routing**: O(n) route matching (can be optimized to trie)
4. **Channel Buffering**: Configurable buffer sizes for handler channels

## Security Considerations

1. **CORS**: Permissive by default, should be configurable
2. **Path Traversal**: Router validates paths
3. **Header Injection**: Headers are properly escaped
4. **Body Size Limits**: Should add configurable limits

## Integration with VM

Currently, route handlers return placeholder responses. Full integration requires:

1. Store handler function references in VM
2. Execute handler functions when routes match
3. Convert between Value and HTTP types
4. Handle async function execution
5. Manage handler lifecycles

This will enable true dynamic request handling with user-defined functions.