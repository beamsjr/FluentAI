//! Web service template

use super::{helpers, Template, TemplateCategory, TemplateOptions};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct WebServiceTemplate;

impl Template for WebServiceTemplate {
    fn name(&self) -> &'static str {
        "webservice"
    }

    fn description(&self) -> &'static str {
        "Basic web service with HTTP routing"
    }

    fn aliases(&self) -> Vec<&'static str> {
        vec!["web-service", "http"]
    }

    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }

    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // Create project file
        helpers::create_project_file(
            path,
            name,
            "Exe",
            &[("FluentAI.Http", "1.0.0"), ("FluentAI.Json", "1.0.0")],
        )?;

        // Create main program
        let program_content = r#";; FluentAI Web Service

(import "fluentai/http" :as http)
(import "fluentai/json" :as json)

;; Define routes
(define routes
  [(http/get "/" home-handler)
   (http/get "/api/hello" hello-handler)
   (http/post "/api/echo" echo-handler)])

;; Handlers
(define home-handler (req)
  (http/response 200 
    :body "Welcome to FluentAI Web Service!"
    :content-type "text/plain"))

(define hello-handler (req)
  (http/json-response 
    {:message "Hello, World!"
     :timestamp (current-time)}))

(define echo-handler (req)
  (let ([body (http/request-body req)])
    (http/json-response body)))

;; Main entry point
(define main (args)
  (let ([port (or (env "PORT") 8080)])
    (println (format "Starting server on port {}" port))
    (http/serve routes :port port)))

;; Run if main module
(when (= __name__ "__main__")
  (main (command-line-args)))
"#;
        fs::write(path.join("Program.ai"), program_content)?;

        // Create directories
        helpers::create_directories(
            path,
            &["src", "src/handlers", "src/middleware", "tests", "static"],
        )?;

        // Create middleware
        let middleware_content = r#";; HTTP middleware

(module middleware
  (import "fluentai/http" :as http)
  (import "fluentai/logger" :as log)
  
  ;; Logging middleware
  (define logging (handler)
    (lambda (req)
      (let ([start-time (current-time-ms)]
            [method (http/request-method req)]
            [path (http/request-path req)])
        (log/info (format "{} {}" method path))
        (let ([response (handler req)]
              [duration (- (current-time-ms) start-time)])
          (log/info (format "{} {} - {} {}ms"
                           method path 
                           (http/response-status response)
                           duration))
          response))))
  
  ;; CORS middleware
  (define cors (handler)
    (lambda (req)
      (let ([response (handler req)])
        (http/add-headers response
          {"Access-Control-Allow-Origin" "*"
           "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
           "Access-Control-Allow-Headers" "Content-Type"}))))
  
  (export logging cors))
"#;
        fs::write(path.join("src/middleware/common.ai"), middleware_content)?;

        // Create a handler module
        let users_handler = r#";; User handlers

(module users
  (import "fluentai/http" :as http)
  (import "fluentai/json" :as json)
  
  ;; In-memory user store for demo
  (define users (atom []))
  
  ;; List all users
  (define list-users (req)
    (http/json-response @users))
  
  ;; Get user by id
  (define get-user (req)
    (let ([id (http/route-param req :id)]
          [user (find (lambda (u) (= (get u :id) id)) @users)])
      (if user
          (http/json-response user)
          (http/json-response {:error "User not found"} :status 404))))
  
  ;; Create new user
  (define create-user (req)
    (let ([body (http/request-json req)]
          [id (generate-id)]
          [user (assoc body :id id)])
      (swap! users conj user)
      (http/json-response user :status 201)))
  
  ;; Helper to generate IDs
  (define generate-id ()
    (str (random-uuid)))
  
  (export list-users get-user create-user))
"#;
        fs::write(path.join("src/handlers/users.ai"), users_handler)?;

        // Create tests
        let test_content = r#";; Web service tests

(import "fluentai/test" :as test)
(import "fluentai/http/test" :as http-test)

(test/describe "Web Service"
  
  (test/describe "Home endpoint"
    (test/it "returns welcome message"
      (let ([response (http-test/get "/")])
        (test/expect (get response :status) :to-equal 200)
        (test/expect (get response :body) 
                     :to-contain "Welcome"))))
  
  (test/describe "API endpoints"
    (test/it "hello endpoint returns JSON"
      (let ([response (http-test/get "/api/hello")]
            [body (json/parse (get response :body))])
        (test/expect (get response :status) :to-equal 200)
        (test/expect (get body :message) :to-equal "Hello, World!")))
    
    (test/it "echo endpoint echoes input"
      (let ([input {:test "data"}]
            [response (http-test/post "/api/echo" :json input)]
            [body (json/parse (get response :body))])
        (test/expect body :to-equal input)))))
"#;
        fs::write(path.join("tests/api.test.ai"), test_content)?;

        // Create static file
        let index_html = r#"<!DOCTYPE html>
<html>
<head>
    <title>FluentAI Web Service</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .container { max-width: 800px; margin: 0 auto; }
        code { background: #f4f4f4; padding: 2px 4px; }
    </style>
</head>
<body>
    <div class="container">
        <h1>FluentAI Web Service</h1>
        <p>Your web service is running!</p>
        
        <h2>API Endpoints</h2>
        <ul>
            <li><code>GET /</code> - This page</li>
            <li><code>GET /api/hello</code> - Hello endpoint</li>
            <li><code>POST /api/echo</code> - Echo endpoint</li>
        </ul>
    </div>
</body>
</html>
"#;
        fs::write(path.join("static/index.html"), index_html)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        // Create README
        helpers::create_readme(path, name, "A FluentAI web service.")?;

        Ok(())
    }
}
