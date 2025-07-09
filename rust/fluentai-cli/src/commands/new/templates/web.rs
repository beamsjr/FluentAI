//! Full-stack web application template

use super::{Template, TemplateCategory, TemplateOptions, TemplateOption, helpers};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct WebTemplate;

impl Template for WebTemplate {
    fn name(&self) -> &'static str {
        "web"
    }
    
    fn description(&self) -> &'static str {
        "Full-stack web application with server-side rendering"
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Application
    }
    
    fn options(&self) -> Vec<TemplateOption> {
        vec![
            TemplateOption {
                name: "frontend",
                description: "Frontend framework",
                default: Some("htmx"),
                choices: vec!["htmx", "alpine", "vue", "react"],
            },
        ]
    }
    
    fn create(&self, path: &Path, name: &str, options: &TemplateOptions) -> Result<()> {
        let frontend = options.frontend.as_deref().unwrap_or("htmx");
        
        // Create project file
        helpers::create_project_file(
            path,
            name,
            "Exe",
            &[
                ("FluentAI.Http", "1.0.0"),
                ("FluentAI.Templates", "1.0.0"),
                ("FluentAI.Sessions", "1.0.0"),
                ("FluentAI.Static", "1.0.0"),
            ],
        )?;
        
        // Create main program
        let program_content = r#";; Full-stack Web Application

(import "fluentai/http" :as http)
(import "fluentai/templates" :as templates)
(import "fluentai/sessions" :as sessions)
(import "fluentai/static" :as static)
(import "./src/routes" :as routes)
(import "./src/config" :as config)

;; Initialize application
(define init-app ()
  ;; Configure templates
  (templates/configure
    :directory "views"
    :cache (config/production?))
  
  ;; Configure sessions
  (sessions/configure
    :secret (config/get :session-secret)
    :max-age (* 24 60 60))) ;; 24 hours

;; Main entry point
(define main (args)
  (init-app)
  
  (let ([app (http/create-app)]
        [port (config/get :port 3000)])
    
    ;; Global middleware
    (http/use app (sessions/middleware))
    (http/use app (http/logger))
    
    ;; Static files
    (http/use app "/static" (static/serve "public"))
    
    ;; Routes
    (http/use app routes/all)
    
    ;; Error pages
    (http/error app 404 routes/not-found)
    (http/error app 500 routes/server-error)
    
    ;; Start server
    (println (format "🌐 Web application running at http://localhost:{}" port))
    (http/listen app :port port)))

(when (= __name__ "__main__")
  (main (command-line-args)))
"#;
        fs::write(path.join("Program.ai"), program_content)?;
        
        // Create directories
        helpers::create_directories(path, &[
            "src",
            "src/controllers",
            "src/models",
            "src/services",
            "views",
            "views/layouts",
            "views/partials",
            "public",
            "public/css",
            "public/js",
            "public/images",
            "tests",
        ])?;
        
        // Create config
        let config_content = r#";; Application configuration

(module config
  (import "fluentai/env" :as env)
  
  (define get
    ([key] (env/get (str/upper-case (str key))))
    ([key default] (or (get key) default)))
  
  (define production? ()
    (= (get :environment "development") "production"))
  
  (export get production?))
"#;
        fs::write(path.join("src/config.ai"), config_content)?;
        
        // Create routes
        let routes_content = r#";; Application routes

(module routes
  (import "fluentai/http" :as http)
  (import "./controllers/home" :as home)
  (import "./controllers/about" :as about)
  (import "./controllers/contact" :as contact)
  
  ;; Route definitions
  (define all
    (http/router
      ;; Home
      (http/get "/" home/index)
      
      ;; About
      (http/get "/about" about/index)
      
      ;; Contact
      (http/get "/contact" contact/index)
      (http/post "/contact" contact/submit)))
  
  ;; Error handlers
  (define not-found (req)
    (http/render "404.html" 
                 {:title "Page Not Found"
                  :path (http/path req)}
                 :status 404))
  
  (define server-error (req error)
    (http/render "500.html"
                 {:title "Server Error"
                  :error (if (config/production?) 
                           "Something went wrong"
                           (str error))}
                 :status 500))
  
  (export all not-found server-error))
"#;
        fs::write(path.join("src/routes.ai"), routes_content)?;
        
        // Create home controller
        let home_controller = r#";; Home controller

(module home
  (import "fluentai/http" :as http)
  
  (define index (req)
    (http/render "home.html"
                 {:title "Welcome"
                  :features ["Fast" "Secure" "Scalable"]}))
  
  (export index))
"#;
        fs::write(path.join("src/controllers/home.ai"), home_controller)?;
        
        // Create about controller
        let about_controller = r#";; About controller

(module about
  (import "fluentai/http" :as http)
  
  (define index (req)
    (http/render "about.html"
                 {:title "About Us"
                  :content "We build amazing web applications with FluentAI."}))
  
  (export index))
"#;
        fs::write(path.join("src/controllers/about.ai"), about_controller)?;
        
        // Create contact controller
        let contact_controller = r#";; Contact controller

(module contact
  (import "fluentai/http" :as http)
  (import "fluentai/validation" :as v)
  (import "fluentai/mail" :as mail)
  
  ;; Contact form schema
  (define contact-schema
    (v/object
      {:name (v/pipe (v/string) (v/min-length 2))
       :email (v/pipe (v/string) (v/email))
       :message (v/pipe (v/string) (v/min-length 10))}))
  
  ;; Show contact form
  (define index (req)
    (http/render "contact.html"
                 {:title "Contact Us"
                  :csrf-token (http/csrf-token req)}))
  
  ;; Handle form submission
  (define submit (req)
    (let ([data (http/form-data req)]
          [errors (v/validate contact-schema data)])
      (if (empty? errors)
          ;; Valid submission
          (do
            ;; Send email (configure mail service first)
            ;; (mail/send ...)
            (http/flash req :success "Thank you for your message!")
            (http/redirect "/contact"))
          ;; Validation errors
          (http/render "contact.html"
                       {:title "Contact Us"
                        :errors errors
                        :values data
                        :csrf-token (http/csrf-token req)}))))
  
  (export index submit))
"#;
        fs::write(path.join("src/controllers/contact.ai"), contact_controller)?;
        
        // Create base layout
        let layout_content = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ title }} - {{ app_name }}</title>
    <link rel="stylesheet" href="/static/css/style.css">
    {% block head %}{% endblock %}
</head>
<body>
    <nav class="navbar">
        <div class="container">
            <a href="/" class="brand">{{ app_name }}</a>
            <ul class="nav-links">
                <li><a href="/">Home</a></li>
                <li><a href="/about">About</a></li>
                <li><a href="/contact">Contact</a></li>
            </ul>
        </div>
    </nav>
    
    {% if flash_messages %}
    <div class="container">
        {% for type, message in flash_messages %}
        <div class="alert alert-{{ type }}">{{ message }}</div>
        {% endfor %}
    </div>
    {% endif %}
    
    <main class="container">
        {% block content %}{% endblock %}
    </main>
    
    <footer>
        <div class="container">
            <p>&copy; 2024 {{ app_name }}. Built with FluentAI.</p>
        </div>
    </footer>
    
    {% block scripts %}{% endblock %}
</body>
</html>
"#;
        fs::write(path.join("views/layouts/base.html"), layout_content)?;
        
        // Create home view
        let home_view = r#"{% extends "layouts/base.html" %}

{% block content %}
<div class="hero">
    <h1>Welcome to {{ app_name }}</h1>
    <p>Build amazing web applications with FluentAI</p>
</div>

<div class="features">
    <h2>Features</h2>
    <div class="feature-grid">
        {% for feature in features %}
        <div class="feature-card">
            <h3>{{ feature }}</h3>
        </div>
        {% endfor %}
    </div>
</div>
{% endblock %}
"#;
        fs::write(path.join("views/home.html"), home_view)?;
        
        // Create about view
        let about_view = r#"{% extends "layouts/base.html" %}

{% block content %}
<h1>{{ title }}</h1>
<p>{{ content }}</p>
{% endblock %}
"#;
        fs::write(path.join("views/about.html"), about_view)?;
        
        // Create contact view
        let contact_view = r#"{% extends "layouts/base.html" %}

{% block content %}
<h1>{{ title }}</h1>

<form method="POST" action="/contact" class="contact-form">
    <input type="hidden" name="_csrf" value="{{ csrf_token }}">
    
    <div class="form-group">
        <label for="name">Name</label>
        <input type="text" id="name" name="name" value="{{ values.name }}" required>
        {% if errors.name %}
        <div class="error">{{ errors.name }}</div>
        {% endif %}
    </div>
    
    <div class="form-group">
        <label for="email">Email</label>
        <input type="email" id="email" name="email" value="{{ values.email }}" required>
        {% if errors.email %}
        <div class="error">{{ errors.email }}</div>
        {% endif %}
    </div>
    
    <div class="form-group">
        <label for="message">Message</label>
        <textarea id="message" name="message" rows="5" required>{{ values.message }}</textarea>
        {% if errors.message %}
        <div class="error">{{ errors.message }}</div>
        {% endif %}
    </div>
    
    <button type="submit" class="btn btn-primary">Send Message</button>
</form>
{% endblock %}
"#;
        fs::write(path.join("views/contact.html"), contact_view)?;
        
        // Create 404 page
        let not_found_view = r#"{% extends "layouts/base.html" %}

{% block content %}
<div class="error-page">
    <h1>404 - Page Not Found</h1>
    <p>The page "{{ path }}" could not be found.</p>
    <a href="/" class="btn">Go Home</a>
</div>
{% endblock %}
"#;
        fs::write(path.join("views/404.html"), not_found_view)?;
        
        // Create 500 page
        let error_view = r#"{% extends "layouts/base.html" %}

{% block content %}
<div class="error-page">
    <h1>500 - Server Error</h1>
    <p>{{ error }}</p>
    <a href="/" class="btn">Go Home</a>
</div>
{% endblock %}
"#;
        fs::write(path.join("views/500.html"), error_view)?;
        
        // Create CSS
        let css_content = r#"/* Base styles */
:root {
    --primary-color: #2563eb;
    --text-color: #1f2937;
    --bg-color: #ffffff;
    --border-color: #e5e7eb;
}

* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    line-height: 1.6;
    color: var(--text-color);
    background-color: var(--bg-color);
}

.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 0 20px;
}

/* Navigation */
.navbar {
    background-color: var(--bg-color);
    border-bottom: 1px solid var(--border-color);
    padding: 1rem 0;
}

.navbar .container {
    display: flex;
    justify-content: space-between;
    align-items: center;
}

.brand {
    font-size: 1.5rem;
    font-weight: bold;
    color: var(--primary-color);
    text-decoration: none;
}

.nav-links {
    display: flex;
    list-style: none;
    gap: 2rem;
}

.nav-links a {
    color: var(--text-color);
    text-decoration: none;
    transition: color 0.3s;
}

.nav-links a:hover {
    color: var(--primary-color);
}

/* Main content */
main {
    min-height: calc(100vh - 200px);
    padding: 2rem 0;
}

/* Hero section */
.hero {
    text-align: center;
    padding: 4rem 0;
}

.hero h1 {
    font-size: 3rem;
    margin-bottom: 1rem;
}

/* Features */
.features {
    margin-top: 4rem;
}

.feature-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 2rem;
    margin-top: 2rem;
}

.feature-card {
    padding: 2rem;
    border: 1px solid var(--border-color);
    border-radius: 8px;
    text-align: center;
}

/* Forms */
.contact-form {
    max-width: 600px;
    margin: 2rem 0;
}

.form-group {
    margin-bottom: 1.5rem;
}

.form-group label {
    display: block;
    margin-bottom: 0.5rem;
    font-weight: 500;
}

.form-group input,
.form-group textarea {
    width: 100%;
    padding: 0.75rem;
    border: 1px solid var(--border-color);
    border-radius: 4px;
    font-size: 1rem;
}

.form-group input:focus,
.form-group textarea:focus {
    outline: none;
    border-color: var(--primary-color);
}

.error {
    color: #ef4444;
    font-size: 0.875rem;
    margin-top: 0.25rem;
}

/* Buttons */
.btn {
    display: inline-block;
    padding: 0.75rem 1.5rem;
    background-color: var(--primary-color);
    color: white;
    text-decoration: none;
    border: none;
    border-radius: 4px;
    font-size: 1rem;
    cursor: pointer;
    transition: background-color 0.3s;
}

.btn:hover {
    background-color: #1d4ed8;
}

.btn-primary {
    background-color: var(--primary-color);
}

/* Alerts */
.alert {
    padding: 1rem;
    margin: 1rem 0;
    border-radius: 4px;
}

.alert-success {
    background-color: #d1fae5;
    color: #065f46;
}

.alert-error {
    background-color: #fee2e2;
    color: #991b1b;
}

/* Error pages */
.error-page {
    text-align: center;
    padding: 4rem 0;
}

.error-page h1 {
    font-size: 2.5rem;
    margin-bottom: 1rem;
}

/* Footer */
footer {
    background-color: #f9fafb;
    border-top: 1px solid var(--border-color);
    padding: 2rem 0;
    text-align: center;
    color: #6b7280;
}
"#;
        fs::write(path.join("public/css/style.css"), css_content)?;
        
        // Create JavaScript file based on frontend choice
        let js_content = match frontend {
            "htmx" => r#"// HTMX is loaded via CDN in the layout
// Add any custom JavaScript here
"#,
            "alpine" => r#"// Alpine.js is loaded via CDN in the layout
// Add Alpine components here
"#,
            _ => r#"// Add your JavaScript here
document.addEventListener('DOMContentLoaded', function() {
    console.log('FluentAI Web Application loaded');
});
"#,
        };
        fs::write(path.join("public/js/app.js"), js_content)?;
        
        // Update layout for frontend framework
        if frontend == "htmx" || frontend == "alpine" {
            let layout_path = path.join("views/layouts/base.html");
            let layout = fs::read_to_string(&layout_path)?;
            let cdn_script = match frontend {
                "htmx" => r#"    <script src="https://unpkg.com/htmx.org@1.9.10"></script>"#,
                "alpine" => r#"    <script defer src="https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"></script>"#,
                _ => "",
            };
            let updated_layout = layout.replace(
                "    {% block scripts %}{% endblock %}",
                &format!("{}\n    <script src=\"/static/js/app.js\"></script>\n    {{% block scripts %}}{{% endblock %}}", cdn_script)
            );
            fs::write(layout_path, updated_layout)?;
        }
        
        // Create .env.example
        let env_example = r#"# Environment configuration
ENVIRONMENT=development
PORT=3000
SESSION_SECRET=your-secret-key-change-in-production
"#;
        fs::write(path.join(".env.example"), env_example)?;
        
        // Create test
        let test_content = r#";; Web application tests

(import "fluentai/test" :as test)
(import "fluentai/http/test" :as http)

(test/describe "Web Application"
  
  (test/describe "Home page"
    (test/it "renders successfully"
      (let [res (http/get "/")]
        (test/expect (:status res) :to-be 200)
        (test/expect (:body res) :to-contain "Welcome"))))
  
  (test/describe "Contact form"
    (test/it "shows form"
      (let [res (http/get "/contact")]
        (test/expect (:status res) :to-be 200)
        (test/expect (:body res) :to-contain "Contact Us")))
    
    (test/it "validates input"
      (let [res (http/post "/contact"
                           :form {:name "A"
                                  :email "invalid"
                                  :message "Hi"})]
        (test/expect (:body res) :to-contain "error")))))
"#;
        fs::write(path.join("tests/web.test.ai"), test_content)?;
        
        // Create README
        helpers::create_readme(
            path,
            name,
            &format!("A full-stack web application built with FluentAI and {}.", frontend)
        )?;
        
        // Create .gitignore
        helpers::create_gitignore(path)?;
        
        Ok(())
    }
}