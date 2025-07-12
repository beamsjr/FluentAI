# HTTP Patterns Documentation

## Overview

FluentAI provides comprehensive HTTP functionality through several modules:
- `http.flc` - Basic HTTP client and server operations
- `rest.flc` - Advanced REST client with retry logic and CRUD patterns
- `oauth2.flc` - OAuth2 authentication flows

## REST Client Module

The REST client module provides a high-level interface for making HTTP requests with automatic retry, timeout handling, and JSON serialization.

### Basic Usage

```fluentai
use rest;

// Create a REST client
let client = rest.client("https://api.example.com")
    .with_header("User-Agent", "MyApp/1.0")
    .with_timeout(10000)  // 10 seconds
    .with_auth("my-api-token");

// Make requests
let users = client.get("/users").await();
let user = client.get("/users/123").await();
let created = client.post("/users", {"name": "Alice"}).await();
let updated = client.put("/users/123", {"name": "Alice Updated"}).await();
let deleted = client.delete("/users/123").await();
```

### Retry Configuration

The REST client supports automatic retry with exponential backoff:

```fluentai
let client = rest.client("https://api.example.com")
    .with_retry(rest.RetryConfig {
        max_retries: 5,
        initial_delay: 100,      // Start with 100ms delay
        max_delay: 10000,        // Cap at 10 seconds
        exponential_base: 2.0    // Double delay each retry
    });
```

Retries occur on:
- Network errors
- 5xx server errors
- Timeouts

### Response Handling

```fluentai
let response = client.get("/data").await();

if response.success {
    // Access parsed JSON data
    let data = response.json();
    
    // Or with default value
    let data = response.json_or({});
} else {
    $(f"Request failed: {response.error}").print();
}

// Response structure:
// - success: bool
// - status: int (HTTP status code)
// - headers: Map
// - body: string (raw response body)
// - data: any? (parsed JSON if applicable)
// - error: string? (error message if failed)
// - duration_ms: int (request duration)
```

### CRUD Client Pattern

For RESTful resources, use the CRUD client:

```fluentai
// Define your resource type
struct User {
    id: string?,
    name: string,
    email: string
}

// Create CRUD client
let users = rest.crud_client<User>("https://api.example.com", "users");

// CRUD operations
let all_users = users.list({"page": 1, "limit": 20}).await();
let user = users.get("123").await();
let new_user = users.create(User{name: "Bob", email: "bob@example.com"}).await();
let updated = users.update("123", User{name: "Bob Updated", email: "bob@example.com"}).await();
let patched = users.patch("123", {"name": "Bob Patched"}).await();
let success = users.delete("123").await();
```

### Interceptors

Add request/response interceptors for cross-cutting concerns:

```fluentai
// Logging interceptor
let logging = rest.logging_interceptor("[API]");

// Timestamp interceptor
let timestamp = rest.timestamp_interceptor();

// Custom interceptor
let custom = rest.Interceptor {
    name: "custom",
    on_request: (req) => {
        // Modify request before sending
        req.headers["X-Custom"] = "value";
        req
    },
    on_response: (res) => {
        // Process response after receiving
        if !res.success {
            // Log errors
        }
        res
    }
};
```

## OAuth2 Module

The OAuth2 module provides complete OAuth2 client implementation.

### Authorization Code Flow

```fluentai
use oauth2;

// Configure OAuth2 client
let config = oauth2.OAuth2Config {
    client_id: "your-client-id",
    client_secret: "your-client-secret",
    auth_url: "https://provider.com/oauth/authorize",
    token_url: "https://provider.com/oauth/token",
    redirect_uri: "http://localhost:8080/callback",
    scopes: ["read", "write"]
};

let oauth_client = oauth2.oauth2_client(config);

// Step 1: Generate authorization URL
let auth_url = oauth_client.auth_url("random-state");
// Redirect user to auth_url

// Step 2: Exchange authorization code for token
let token = oauth_client.exchange_code(auth_code).await();

// Step 3: Use token for API requests
let api_client = oauth_client.rest_client("https://api.provider.com").await();
let data = api_client.get("/user").await();
```

### Client Credentials Flow

```fluentai
// For server-to-server authentication
let token = oauth_client.client_credentials().await();
```

### Token Refresh

```fluentai
// Refresh expired token
if token.is_expired() {
    let new_token = oauth_client.refresh_token(token.refresh_token).await();
}

// Or let the client handle it automatically
let api_client = oauth_client.rest_client("https://api.provider.com").await();
// Client will automatically refresh token if needed
```

### Pre-configured Providers

```fluentai
// GitHub
let github = oauth2.github_oauth2(client_id, client_secret, redirect_uri);

// Google
let google = oauth2.google_oauth2(client_id, client_secret, redirect_uri);

// Microsoft
let microsoft = oauth2.microsoft_oauth2(client_id, client_secret, redirect_uri);
```

### Token Storage

```fluentai
// In-memory storage
let storage = oauth2.MemoryTokenStorage { tokens: {} };

// File-based storage
let storage = oauth2.FileTokenStorage { directory: "/tmp/tokens" };

// Save token
storage.save_token("github", token).await();

// Load token
let saved_token = storage.load_token("github").await();
```

## Common Patterns

### API Client with Authentication

```fluentai
use rest;
use oauth2;

struct APIClient {
    oauth: oauth2.OAuth2Client,
    base_url: string
}

private APIClient {
    async function make_request(self, method: string, path: string, data: any?) -> any {
        // Get REST client with current token
        let client = self.oauth.rest_client(self.base_url).await();
        
        // Make request based on method
        let response = match method {
            "GET" => client.get(path).await(),
            "POST" => client.post(path, data).await(),
            "PUT" => client.put(path, data).await(),
            "DELETE" => client.delete(path).await(),
            _ => error("Unsupported method")
        };
        
        if response.success {
            response.json()
        } else {
            error(f"API request failed: {response.error}")
        }
    }
}
```

### Pagination Helper

```fluentai
async function fetch_all_pages<T>(client: rest.RestClient, path: string) -> List<T> {
    let mut all_items = [];
    let mut page = 1;
    
    loop {
        let response = client.get(path, {"page": page, "per_page": 100}).await();
        let paged = rest.parse_pagination<T>(response);
        
        all_items.extend(paged.items);
        
        if !paged.has_next {
            break;
        }
        
        page += 1;
    }
    
    all_items
}
```

### Error Handling with Retries

```fluentai
async function resilient_api_call(client: rest.RestClient, path: string) -> any? {
    let response = client
        .with_retry(rest.RetryConfig {
            max_retries: 5,
            initial_delay: 1000,
            max_delay: 30000,
            exponential_base: 2.0
        })
        .get(path)
        .await();
    
    if response.success {
        response.json()
    } else {
        // Log error but don't crash
        $(f"API call failed after retries: {response.error}").print();
        null
    }
}
```

### Batch Requests

```fluentai
async function batch_create(client: rest.RestClient, items: List<any>) -> List<any> {
    // Process in parallel with concurrency limit
    let results = items
        .chunks(10)  // Process 10 at a time
        .map_async(chunk => {
            chunk.map_async(item => {
                client.post("/items", item).await()
            }).await()
        })
        .await()
        .flatten();
    
    // Extract successful results
    results
        .filter(r => r.success)
        .map(r => r.json())
}
```

## Best Practices

1. **Always Set Timeouts**: Default timeouts prevent hanging requests
   ```fluentai
   let client = rest.client(url).with_timeout(10000);  // 10 seconds
   ```

2. **Use Retry for Idempotent Operations**: GET, PUT, DELETE are safe to retry
   ```fluentai
   let client = rest.client(url).with_retry(rest.default_retry_config());
   ```

3. **Handle Errors Gracefully**: Check response.success before using data
   ```fluentai
   if response.success {
       // Use response.json()
   } else {
       // Handle error
   }
   ```

4. **Store OAuth2 Tokens Securely**: Use appropriate storage based on environment
   ```fluentai
   // Development: Memory storage
   // Production: Encrypted file or database storage
   ```

5. **Use Type-Safe CRUD Clients**: Define your resource types
   ```fluentai
   let users = rest.crud_client<User>(url, "users");
   ```

## Performance Tips

1. **Reuse Clients**: Create once and reuse for multiple requests
2. **Parallel Requests**: Use map_async for concurrent operations
3. **Connection Pooling**: Handled automatically by the underlying implementation
4. **Caching**: Implement at application level based on your needs

## Examples

See the following example files for complete implementations:
- `/examples/rest_client_example.flc` - REST client usage
- `/examples/github_api_client.flc` - OAuth2 with GitHub API
- `/examples/websocket_chat.flc` - WebSocket chat server
- `/examples/websocket_echo.flc` - Simple WebSocket echo server