//! RESTful API template

use super::{helpers, Template, TemplateCategory, TemplateOption, TemplateOptions};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct ApiTemplate;

impl Template for ApiTemplate {
    fn name(&self) -> &'static str {
        "api"
    }

    fn description(&self) -> &'static str {
        "RESTful API with authentication, database, and OpenAPI documentation"
    }

    fn aliases(&self) -> Vec<&'static str> {
        vec!["rest", "restful"]
    }

    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }

    fn options(&self) -> Vec<TemplateOption> {
        vec![
            TemplateOption {
                name: "auth",
                description: "Authentication type",
                default: Some("jwt"),
                choices: vec!["jwt", "oauth", "basic", "none"],
            },
            TemplateOption {
                name: "database",
                description: "Database type",
                default: Some("postgres"),
                choices: vec!["postgres", "mysql", "sqlite", "mongodb", "none"],
            },
        ]
    }

    fn create(&self, path: &Path, name: &str, options: &TemplateOptions) -> Result<()> {
        let auth_type = options.auth.as_deref().unwrap_or("jwt");
        let db_type = options.database.as_deref().unwrap_or("postgres");

        // Create project file
        let mut packages = vec![
            ("FluentAI.Http", "1.0.0"),
            ("FluentAI.Json", "1.0.0"),
            ("FluentAI.Validation", "1.0.0"),
            ("FluentAI.OpenAPI", "1.0.0"),
        ];

        if auth_type != "none" {
            packages.push(("FluentAI.Auth", "1.0.0"));
        }

        if db_type != "none" {
            packages.push(("FluentAI.Database", "1.0.0"));
        }

        helpers::create_project_file(path, name, "Exe", &packages)?;

        // Create main program
        let program_content = format!(
            r#"// main.flc
// {} API - RESTful API with authentication and database

use std::http::{{create_app, use, mount, listen}};
use std::json;
use std::openapi;
use ./src/config;
use ./src/database as db;
use ./src/auth;
use ./src/routes;
use ./src/middleware;

// Initialize application
private async function init_app() {{
    // Load configuration
    config.load();
    
    // Connect to database
    {};
    
    // Initialize OpenAPI documentation
    openapi.init({{
        "title": "{} API",
        "version": "1.0.0",
        "description": "RESTful API built with FluentAI"
    }});
}}

// Main entry point
private async function main() {{
    init_app().await();
    
    let app = create_app();
    let port = config.get("port", 8080);
    
    // Apply global middleware
    app.use(middleware.error_handler());
    app.use(middleware.request_logger());
    app.use(middleware.cors());
    {};
    
    // Mount routes
    app.mount("/api/v1", routes.api_v1());
    app.mount("/docs", openapi.ui());
    app.mount("/openapi.json", openapi.spec());
    
    // Start server
    $(f"🚀 {} API running on port {{port}}").print();
    app.listen(port).await();
}}

// Run the application
main()
"#,
            name,
            if db_type != "none" {
                "db.connect().await()"
            } else {
                "// No database configured"
            },
            name,
            if auth_type != "none" {
                "app.use(middleware.authenticate())"
            } else {
                ""
            },
            name
        );
        fs::write(path.join("main.flc"), program_content)?;

        // Create directories
        helpers::create_directories(
            path,
            &[
                "src",
                "src/models",
                "src/routes",
                "src/services",
                "src/validators",
                "tests",
                "tests/integration",
                "tests/unit",
                "migrations",
                "scripts",
            ],
        )?;

        // Create config module
        let config_content = r#"// config.flc
module config;

use std::env;

// Configuration state
private let config_state = {
    "port": 8080,
    "env": "development",
    "database_url": "",
    "jwt_secret": "change-me-in-production",
    "cors_origin": "*",
    "log_level": "info"
};

// Load configuration from environment
public function load() {
    config_state.port = env.get("PORT", 8080);
    config_state.env = env.get("NODE_ENV", "development");
    config_state.database_url = env.get("DATABASE_URL", "");
    config_state.jwt_secret = env.get("JWT_SECRET", "change-me-in-production");
    config_state.cors_origin = env.get("CORS_ORIGIN", "*");
    config_state.log_level = env.get("LOG_LEVEL", "info");
}

// Get configuration value
public function get(key: string, default_value?) {
    config_state.get(key).unwrap_or(default_value)
}

// Check if production
public function is_production() -> bool {
    config_state.env == "production"
}
"#;
        fs::write(path.join("src/config.flc"), config_content)?;

        // Create database module
        let db_content = if db_type != "none" {
            format!(
                r#"// database.flc
module database;

use std::db;
use std::db::{} as driver;
use ./config;

// Connection state
private let connection = null;

// Connect to database
public async function connect() {{
    let url = config.get("database_url");
    if (!url) {{
        throw Error("DATABASE_URL not configured");
    }}
    connection = driver.connect(url).await();
    $("✓ Database connected").print();
}}

// Get connection
public function conn() {{
    connection
}}

// Execute query
public async function query(sql: string, ...params) {{
    db.query(conn(), sql, params).await()
}}

// Execute command
public async function execute(sql: string, ...params) {{
    db.execute(conn(), sql, params).await()
}}

// Transaction helper
public async function with_transaction(f: function) {{
    db.with_transaction(conn(), f).await()
}}
"#,
                db_type
            )
        } else {
            r#"// database.flc
module database;

// Database module (no database configured)

// Placeholder for database operations
public async function connect() {
    // No database to connect
}
"#
            .to_string()
        };
        fs::write(path.join("src/database.flc"), db_content)?;

        // Create auth module
        let auth_content = if auth_type != "none" {
            format!(
                r#";; Authentication module

(module auth
  (import "fluentai/auth" :as auth)
  (import "fluentai/auth/{}" :as strategy)
  (import "./config" :as config)
  
  ;; Initialize authentication
  (define init ()
    (auth/configure
      :strategy :{}
      :secret (config/get :jwt-secret)))
  
  ;; Generate token for user
  (define generate-token (user)
    (auth/sign
      {{:id (get user :id)
       :email (get user :email)
       :roles (get user :roles [])}}
      :expires-in "24h"))
  
  ;; Verify token
  (define verify-token (token)
    (auth/verify token))
  
  ;; Authenticate request
  (define authenticate-request (req)
    (let ([token (or (http/header req "authorization")
                     (http/query req "token"))])
      (when token
        (try
          (verify-token (str/replace token "Bearer " ""))
          (catch _ nil)))))
  
  ;; Require authentication middleware
  (define require-auth (handler)
    (lambda (req)
      (let ([user (authenticate-request req)])
        (if user
            (handler (assoc req :user user))
            (http/response 401
              :json {{:error "Unauthorized"}}
              :headers {{"WWW-Authenticate" "Bearer"}})))))
  
  ;; Require specific role
  (define require-role (role)
    (lambda (handler)
      (lambda (req)
        (let ([user (get req :user)])
          (if (and user (contains? (get user :roles []) role))
              (handler req)
              (http/response 403
                :json {{:error "Forbidden"}}))))))
  
  (export init generate-token verify-token authenticate-request 
          require-auth require-role))
"#,
                auth_type, auth_type
            )
        } else {
            r#";; Authentication module (no auth configured)

(module auth
  (define init () nil)
  (define require-auth (handler) handler)
  (export init require-auth))
"#
            .to_string()
        };
        fs::write(path.join("src/auth.ai"), auth_content)?;

        // Create middleware
        let middleware_content = r#";; API middleware

(module middleware
  (import "fluentai/http" :as http)
  (import "fluentai/logger" :as log)
  (import "./auth" :as auth)
  
  ;; Error handler middleware
  (define error-handler (handler)
    (lambda (req)
      (try
        (handler req)
        (catch e
          (log/error "Request error:" e)
          (http/json-response
            {:error (if (config/production?)
                       "Internal server error"
                       (str e))}
            :status 500)))))
  
  ;; Request logger middleware
  (define request-logger (handler)
    (lambda (req)
      (let ([start (current-time-ms)]
            [method (http/method req)]
            [path (http/path req)])
        (log/info (format "{} {}" method path))
        (let ([res (handler req)]
              [duration (- (current-time-ms) start)]
              [status (http/status res)]]
          (log/info (format "{} {} {} {}ms" method path status duration))
          res))))
  
  ;; CORS middleware
  (define cors (handler)
    (lambda (req)
      (if (= (http/method req) "OPTIONS")
          (http/response 204
            :headers {"Access-Control-Allow-Origin" (config/get :cors-origin)
                     "Access-Control-Allow-Methods" "GET,POST,PUT,DELETE,OPTIONS"
                     "Access-Control-Allow-Headers" "Content-Type,Authorization"
                     "Access-Control-Max-Age" "86400"})
          (let ([res (handler req)])
            (http/add-header res
              "Access-Control-Allow-Origin" (config/get :cors-origin))))))
  
  ;; Authentication middleware
  (define authenticate auth/require-auth)
  
  ;; Validation middleware
  (define validate (schema)
    (lambda (handler)
      (lambda (req)
        (let ([body (http/json-body req)]
              [errors (validate-schema schema body)])
          (if (empty? errors)
              (handler req)
              (http/json-response
                {:error "Validation failed"
                 :details errors}
                :status 400))))))
  
  (export error-handler request-logger cors authenticate validate))
"#;
        fs::write(path.join("src/middleware.ai"), middleware_content)?;

        // Create routes
        let routes_content = r#";; API Routes

(module routes
  (import "fluentai/http" :as http)
  (import "./routes/users" :as users)
  (import "./routes/health" :as health)
  
  ;; API v1 routes
  (define api-v1
    (http/router
      ;; Health check
      (http/get "/health" health/check)
      
      ;; User routes
      (http/get "/users" users/list)
      (http/get "/users/:id" users/get)
      (http/post "/users" users/create)
      (http/put "/users/:id" users/update)
      (http/delete "/users/:id" users/delete)
      
      ;; Add more routes here
      ))
  
  (export api-v1))
"#;
        fs::write(path.join("src/routes.ai"), routes_content)?;

        // Create health route
        let health_route = r#";; Health check endpoint

(module health
  (import "fluentai/http" :as http)
  (import "../database" :as db)
  
  (define check (req)
    (let ([db-status (try
                       (db/query "SELECT 1")
                       "healthy"
                       (catch _ "unhealthy"))])
      (http/json-response
        {:status "ok"
         :timestamp (current-time)
         :services {:database db-status}})))
  
  (export check))
"#;
        fs::write(path.join("src/routes/health.ai"), health_route)?;

        // Create users route
        let users_route = r#";; User routes

(module users
  (import "fluentai/http" :as http)
  (import "../services/users" :as users-service)
  (import "../validators/user" :as user-validator)
  (import "../middleware" :as middleware)
  
  ;; List users
  (define list
    (middleware/authenticate
      (lambda (req)
        (let ([page (http/query-param req :page 1)]
              [limit (http/query-param req :limit 20)]
              [users (users-service/find-all :page page :limit limit)])
          (http/json-response users)))))
  
  ;; Get user by ID
  (define get
    (middleware/authenticate
      (lambda (req)
        (let ([id (http/param req :id)]
              [user (users-service/find-by-id id)])
          (if user
              (http/json-response user)
              (http/json-response
                {:error "User not found"}
                :status 404))))))
  
  ;; Create user
  (define create
    (middleware/validate user-validator/create-schema
      (lambda (req)
        (let ([data (http/json-body req)]
              [user (users-service/create data)])
          (http/json-response user :status 201)))))
  
  ;; Update user
  (define update
    (middleware/authenticate
      (middleware/validate user-validator/update-schema
        (lambda (req)
          (let ([id (http/param req :id)]
                [data (http/json-body req)]
                [user (users-service/update id data)])
            (if user
                (http/json-response user)
                (http/json-response
                  {:error "User not found"}
                  :status 404)))))))
  
  ;; Delete user
  (define delete
    (middleware/authenticate
      (lambda (req)
        (let ([id (http/param req :id)]
              [deleted (users-service/delete id)])
          (if deleted
              (http/response 204)
              (http/json-response
                {:error "User not found"}
                :status 404))))))
  
  (export list get create update delete))
"#;
        fs::write(path.join("src/routes/users.ai"), users_route)?;

        // Create user service
        let user_service = r#";; User service

(module users
  (import "../database" :as db)
  (import "../models/user" :as user-model)
  (import "fluentai/crypto" :as crypto)
  
  ;; Find all users
  (define find-all [& {:keys [page limit]}]
    (let ([offset (* (- page 1) limit)])
      (db/query
        "SELECT * FROM users ORDER BY created_at DESC LIMIT ? OFFSET ?"
        limit offset)))
  
  ;; Find user by ID
  (define find-by-id [id]
    (first
      (db/query "SELECT * FROM users WHERE id = ?" id)))
  
  ;; Find user by email
  (define find-by-email [email]
    (first
      (db/query "SELECT * FROM users WHERE email = ?" email)))
  
  ;; Create new user
  (define create [data]
    (let ([user (-> data
                    (assoc :id (generate-uuid))
                    (update :password crypto/hash-password)
                    (assoc :created-at (current-time))
                    (assoc :updated-at (current-time)))])
      (db/execute!
        "INSERT INTO users (id, email, password, name, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?)"
        (:id user) (:email user) (:password user) (:name user)
        (:created-at user) (:updated-at user))
      (dissoc user :password)))
  
  ;; Update user
  (define update [id data]
    (when-let [user (find-by-id id)]
      (let ([updated (-> data
                         (select-keys [:name :email])
                         (assoc :updated-at (current-time)))])
        (db/execute!
          "UPDATE users SET name = ?, email = ?, updated_at = ? WHERE id = ?"
          (:name updated) (:email updated) (:updated-at updated) id)
        (merge user updated))))
  
  ;; Delete user
  (define delete [id]
    (let ([result (db/execute! "DELETE FROM users WHERE id = ?" id)])
      (> (:rows-affected result) 0)))
  
  (export find-all find-by-id find-by-email create update delete))
"#;
        fs::write(path.join("src/services/users.ai"), user_service)?;

        // Create user validator
        let user_validator = r#";; User validation schemas

(module user
  (import "fluentai/validation" :as v)
  
  ;; Schema for creating a user
  (define create-schema
    (v/object
      {:email (v/pipe (v/string) (v/email))
       :password (v/pipe (v/string) (v/min-length 8))
       :name (v/pipe (v/string) (v/min-length 2) (v/max-length 100))}))
  
  ;; Schema for updating a user
  (define update-schema
    (v/object
      {:email (v/optional (v/pipe (v/string) (v/email)))
       :name (v/optional (v/pipe (v/string) (v/min-length 2) (v/max-length 100)))}))
  
  (export create-schema update-schema))
"#;
        fs::write(path.join("src/validators/user.ai"), user_validator)?;

        // Create user model
        let user_model = r#";; User model

(module user
  
  ;; User schema
  (define schema
    {:id :uuid
     :email :string
     :password :string
     :name :string
     :roles [:array :string]
     :created-at :timestamp
     :updated-at :timestamp})
  
  ;; Create table SQL
  (define create-table-sql
    "CREATE TABLE IF NOT EXISTS users (
      id UUID PRIMARY KEY,
      email VARCHAR(255) UNIQUE NOT NULL,
      password VARCHAR(255) NOT NULL,
      name VARCHAR(100) NOT NULL,
      roles TEXT[] DEFAULT '{}',
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )")
  
  ;; Indexes
  (define indexes
    ["CREATE INDEX idx_users_email ON users(email)"
     "CREATE INDEX idx_users_created_at ON users(created_at)"])
  
  (export schema create-table-sql indexes))
"#;
        fs::write(path.join("src/models/user.ai"), user_model)?;

        // Create migration
        let migration = r#";; Initial database migration

(import "../src/models/user" :as user-model)

;; Up migration
(define up [db]
  ;; Create users table
  (db/execute! user-model/create-table-sql)
  
  ;; Create indexes
  (doseq [idx user-model/indexes]
    (db/execute! idx)))

;; Down migration
(define down [db]
  (db/execute! "DROP TABLE IF EXISTS users"))

(export up down)
"#;
        fs::write(path.join("migrations/001_create_users.ai"), migration)?;

        // Create test
        let test_content = r#";; API integration tests

(import "fluentai/test" :as test)
(import "fluentai/http/test" :as http)
(import "../src/auth" :as auth)

(test/describe "API Integration Tests"
  
  (test/before-all
    (fn []
      ;; Start test server
      (http/start-test-server)))
  
  (test/after-all
    (fn []
      ;; Stop test server
      (http/stop-test-server)))
  
  (test/describe "Health endpoint"
    (test/it "returns healthy status"
      (let [res (http/get "/api/v1/health")]
        (test/expect (:status res) :to-be 200)
        (test/expect (get-in res [:body :status]) :to-be "ok"))))
  
  (test/describe "User endpoints"
    (def test-user {:email "test@example.com"
                    :password "password123"
                    :name "Test User"})
    
    (test/it "creates a new user"
      (let [res (http/post "/api/v1/users" :json test-user)]
        (test/expect (:status res) :to-be 201)
        (test/expect (get-in res [:body :email]) :to-be (:email test-user))))
    
    (test/it "requires authentication for user list"
      (let [res (http/get "/api/v1/users")]
        (test/expect (:status res) :to-be 401)))
    
    (test/it "lists users with valid token"
      (let [token (auth/generate-token {:id "test-id" :email "test@example.com"})
            res (http/get "/api/v1/users" 
                         :headers {"Authorization" (str "Bearer " token)})]
        (test/expect (:status res) :to-be 200)
        (test/expect (:body res) :to-be-array)))))
"#;
        fs::write(path.join("tests/integration/api.test.ai"), test_content)?;

        // Create .env.example
        let env_example = format!(
            r#"# Environment configuration

# Server
PORT=8080
NODE_ENV=development

# Database
DATABASE_URL={}

# Authentication
JWT_SECRET=your-secret-key-change-in-production

# CORS
CORS_ORIGIN=http://localhost:3000

# Logging
LOG_LEVEL=info
"#,
            match db_type {
                "postgres" => "postgresql://user:password@localhost/dbname",
                "mysql" => "mysql://user:password@localhost/dbname",
                "sqlite" => "sqlite:./database.db",
                "mongodb" => "mongodb://localhost:27017/dbname",
                _ => "",
            }
        );
        fs::write(path.join(".env.example"), env_example)?;

        // Create docker-compose.yml if database is configured
        if db_type != "none" && options.docker {
            let docker_compose = match db_type {
                "postgres" => {
                    r#"version: '3.8'

services:
  db:
    image: postgres:15
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: api_dev
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
"#
                }
                "mysql" => {
                    r#"version: '3.8'

services:
  db:
    image: mysql:8
    environment:
      MYSQL_ROOT_PASSWORD: root
      MYSQL_DATABASE: api_dev
      MYSQL_USER: mysql
      MYSQL_PASSWORD: mysql
    ports:
      - "3306:3306"
    volumes:
      - mysql_data:/var/lib/mysql

volumes:
  mysql_data:
"#
                }
                _ => "",
            };

            if !docker_compose.is_empty() {
                fs::write(path.join("docker-compose.yml"), docker_compose)?;
            }
        }

        // Create README
        let db_prerequisite = if db_type != "none" {
            format!("- {} Database", capitalize(db_type))
        } else {
            String::new()
        };

        let db_setup = if db_type != "none" && options.docker {
            "4. Start the database:\n\n```bash\ndocker-compose up -d\n```\n\n5. Run migrations:\n\n```bash\nfluentai db migrate\n```"
        } else {
            ""
        };

        let readme_content = format!(
            r#"# {} API

RESTful API built with FluentAI.

## Features

- 🔐 {} Authentication
- 🗄️ {} Database
- 📝 OpenAPI Documentation
- ✅ Input Validation
- 🧪 Integration Tests
- 🚀 Production Ready

## Getting Started

### Prerequisites

- FluentAI SDK 1.0+
{}

### Installation

1. Clone the repository
2. Copy `.env.example` to `.env` and configure
3. Install dependencies:

```bash
fluentai restore
```

{}

### Running

```bash
fluentai run
```

The API will be available at http://localhost:8080

### API Documentation

Once running, visit:
- OpenAPI UI: http://localhost:8080/docs
- OpenAPI Spec: http://localhost:8080/openapi.json

## Development

### Running tests

```bash
fluentai test
```

### Database migrations

```bash
fluentai db migrate
```

### Building for production

```bash
fluentai build -c Release
```

## API Endpoints

### Health Check
- `GET /api/v1/health` - Check API status

### Users
- `GET /api/v1/users` - List users (requires auth)
- `GET /api/v1/users/:id` - Get user by ID (requires auth)
- `POST /api/v1/users` - Create new user
- `PUT /api/v1/users/:id` - Update user (requires auth)
- `DELETE /api/v1/users/:id` - Delete user (requires auth)

## License

MIT
"#,
            name,
            capitalize(auth_type),
            capitalize(db_type),
            db_prerequisite,
            db_setup
        );

        fs::write(path.join("README.md"), readme_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        Ok(())
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
