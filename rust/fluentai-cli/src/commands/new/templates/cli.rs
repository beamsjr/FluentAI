//! Command-line interface template

use super::{helpers, Template, TemplateCategory, TemplateOptions};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct CliTemplate;

impl Template for CliTemplate {
    fn name(&self) -> &'static str {
        "cli"
    }

    fn description(&self) -> &'static str {
        "Command-line tool with argument parsing and subcommands"
    }

    fn aliases(&self) -> Vec<&'static str> {
        vec!["command", "cmd", "tool"]
    }

    fn category(&self) -> TemplateCategory {
        TemplateCategory::Tool
    }

    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // Create project file
        helpers::create_project_file(
            path,
            name,
            "Exe",
            &[("FluentAI.CLI", "1.0.0"), ("FluentAI.Console", "1.0.0")],
        )?;

        // Create main program
        let program_content = format!(
            r#"// main.flc
// {} CLI - Command-line interface application

(import "fluentai/cli" :as cli)
(import "fluentai/console" :as console)
(import "./src/commands" :as commands)

;; CLI application definition
(define app
  (cli/app
    :name "{}"
    :version "1.0.0"
    :description "A command-line tool built with FluentAI"
    :author "Your Name"))

;; Global options
(cli/option app
  :name "verbose"
  :short "-v"
  :long "--verbose"
  :description "Enable verbose output"
  :type :bool)

(cli/option app
  :name "config"
  :short "-c"
  :long "--config"
  :description "Configuration file path"
  :type :string)

;; Commands
(cli/command app "init" commands/init
  :description "Initialize a new project"
  :args [(cli/arg "name" :description "Project name")]
  :options [(cli/opt "template" "-t" "--template" 
                     :description "Project template"
                     :default "default")])

(cli/command app "build" commands/build
  :description "Build the project"
  :options [(cli/opt "release" "-r" "--release"
                     :description "Build in release mode"
                     :type :bool)])

(cli/command app "run" commands/run
  :description "Run the project"
  :args [(cli/arg "args" :description "Arguments to pass" :variadic true)])

(cli/command app "test" commands/test
  :description "Run tests"
  :options [(cli/opt "filter" "-f" "--filter"
                     :description "Filter tests by pattern")])

(cli/command app "clean" commands/clean
  :description "Clean build artifacts")

;; Help customization
(cli/help-template app
  "{{{{description}}}}

Usage:
  {{{{name}}}} [OPTIONS] <COMMAND>

Options:
{{{{options}}}}

Commands:
{{{{commands}}}}

Examples:
  {{{{name}}}} init my-project
  {{{{name}}}} build --release
  {{{{name}}}} run -- --port 8080

For more help on a command, use:
  {{{{name}}}} <COMMAND> --help")

;; Main entry point
(define main (args)
  (cli/run app args))

(when (= __name__ "__main__")
  (main (command-line-args)))
"#,
            name,
            name.to_lowercase()
        );
        fs::write(path.join("Program.ai"), program_content)?;

        // Create directories
        helpers::create_directories(
            path,
            &["src", "src/commands", "src/utils", "tests", "scripts"],
        )?;

        // Create commands module
        let commands_content = r#";; CLI Commands

(module commands
  (import "fluentai/console" :as console)
  (import "fluentai/fs" :as fs)
  (import "../utils/spinner" :as spinner)
  (import "../utils/config" :as config)
  
  ;; Initialize command
  (define init (ctx)
    (let ([name (cli/arg ctx "name")]
          [template (cli/option ctx "template")]
          [verbose (cli/global-option ctx "verbose")])
      
      (when verbose
        (console/log "Creating project:" name "with template:" template))
      
      (let ([spinner (spinner/start "Initializing project...")])
        ;; Create project directory
        (fs/mkdir name)
        
        ;; Create project files
        (fs/write-file (fs/join name "config.toml ")
                       (config/generate-template template))
        
        (fs/write-file (fs/join name "README.md")
                       (format "{} README - Created with template: {}" name template))
        
        (spinner/success spinner "Project initialized!")
        (console/success (format "[OK] Created project: {}" name))
        (console/info "Next steps:")
        (console/info (format "  cd {}" name))
        (console/info "  <cli-name> build "))))
  
  ;; Build command
  (define build (ctx)
    (let ([release (cli/option ctx "release")]
          [config-file (or (cli/global-option ctx "config") "config.toml ")])
      
      (if (fs/exists? config-file)
          (let ([config (config/load config-file)]
                [mode (if release "release" "debug")]
                [spinner (spinner/start (format "Building in {} mode..." mode))])
            
            ;; Simulate build process
            (thread/sleep 1000)
            
            (spinner/success spinner "Build complete!")
            (console/success (format "[OK] Built successfully in {} mode " mode)))
          
          (do
            (console/error "No config.toml found ")
            (console/info "Run init first to create a project ")
            (cli/exit 1)))))
  
  ;; Run command
  (define run (ctx)
    (let ([args (cli/arg ctx "args")]
          [verbose (cli/global-option ctx "verbose")])
      
      (when verbose
        (console/log "Running with args:" args))
      
      (if (fs/exists? "build/app ")
          (do
            (console/info "Running application...")
            ;; Run the built application
            (let ([result (process/run "build/app " args)])
              (cli/exit (:exit-code result))))
          (do
            (console/error "No build found ")
            (console/info "Run build first ")
            (cli/exit 1)))))
  
  ;; Test command
  (define test (ctx)
    (let ([filter (cli/option ctx "filter")]
          [spinner (spinner/start "Running tests...")])
      
      (console/dim "Discovering tests...")
      
      ;; Find test files
      (let ([test-files (if filter
                           (fs/glob (format "*{}*.test " filter))
                           (fs/glob "*.test "))]}
        
        (if (empty? test-files)
            (do
              (spinner/warn spinner "No tests found ")
              (console/warn "No test files found "))
            
            (do
              (spinner/update spinner 
                             (format "Running {} tests..." (count test-files)))
              
              ;; Run tests (simplified)
              (thread/sleep 1500)
              
              (spinner/success spinner "All tests passed!")
              (console/success (format "[OK] {} tests passed " (count test-files))))))))
  
  ;; Clean command
  (define clean (ctx)
    (let ([spinner (spinner/start "Cleaning build artifacts...")])
      
      (when (fs/exists? "build")
        (fs/remove-dir "build"))
      
      (when (fs/exists? ".cache ")
        (fs/remove-dir ".cache "))
      
      (spinner/success spinner "Clean complete!")
      (console/success "[OK] Build artifacts removed ")))
  
  (export init build run test clean))
"#;
        fs::write(path.join("src/commands.ai"), commands_content)?;

        // Create spinner utility
        let spinner_content = r#";; Spinner utility for CLI feedback

(module spinner
  (import "fluentai/console" :as console)
  (import "fluentai/async" :as async)
  
  (define spinner-frames ["|" "/" "-" "\\" "|" "/" "-" "\\" "|" "/"])
  
  ;; Spinner state
  (defrecord Spinner [message task frame])
  
  ;; Start a spinner
  (define start (message)
    (let ([task (async/interval 80 
                  (let ([frame (atom 0)])
                    (fn []
                      (console/write-line 
                        (format "\r{} {}" 
                                (nth spinner-frames (mod @frame 10))
                                message))
                      (swap! frame inc))))]
          [spinner (Spinner. message task 0)])
      spinner))
  
  ;; Update spinner message
  (define update (spinner new-message)
    (assoc spinner :message new-message))
  
  ;; Stop spinner with success
  (define success (spinner message)
    (async/cancel (:task spinner))
    (console/write-line (format "\r{} {}" 
                                (console/green "[OK]") 
                                message)))
  
  ;; Stop spinner with warning
  (define warn (spinner message)
    (async/cancel (:task spinner))
    (console/write-line (format "\r{} {}" 
                                (console/yellow "[!]") 
                                message)))
  
  ;; Stop spinner with error
  (define error (spinner message)
    (async/cancel (:task spinner))
    (console/write-line (format "\r{} {}" 
                                (console/red "[X]") 
                                message)))
  
  (export start update success warn error))
"#;
        fs::write(path.join("src/utils/spinner.ai"), spinner_content)?;

        // Create config utility
        let config_util = r#";; Configuration utilities

(module config
  (import "fluentai/toml" :as toml)
  (import "fluentai/fs" :as fs)
  
  ;; Load configuration from file
  (define load (path)
    (if (fs/exists? path)
        (toml/parse (fs/read-file path))
        {}))
  
  ;; Save configuration to file
  (define save (path config)
    (fs/write-file path (toml/stringify config)))
  
  ;; Generate template configuration
  (define generate-template (template-name)
    (case template-name
      "default" 
      "[project]
name = \"my-project\"
version = \"1.0.0\"
author = \"Your Name\"

[build]
output = \"build/app\"
optimize = true

[dependencies]
# Add dependencies here"
      
      "lib"
      "[library]
name = \"my-library\"
version = \"1.0.0\"
type = \"static\"

[build]
output = \"build/lib\"

[dependencies]
# Add dependencies here"
      
      ;; Default fallback
      "[project]
name = \"my-project\"
version = \"1.0.0\""))
  
  (export load save generate-template))
"#;
        fs::write(path.join("src/utils/config.ai"), config_util)?;

        // Create test
        let test_content = r#";; CLI tests

(import "fluentai/test" :as test)
(import "fluentai/cli/test" :as cli-test)

(test/describe "CLI Application"
  
  (test/describe "Init command"
    (test/it "creates project directory"
      (cli-test/with-temp-dir
        (fn [dir]
          (let [result (cli-test/run ["init" "test-project "])]
            (test/expect (:exit-code result) :to-be 0)
            (test/expect (fs/exists? "test-project ") :to-be true)
            (test/expect (fs/exists? "test-project/config.toml ") :to-be true))))))
  
  (test/describe "Build command"
    (test/it "requires config file"
      (cli-test/with-temp-dir
        (fn [dir]
          (let [result (cli-test/run ["build"])]
            (test/expect (:exit-code result) :to-be 1)
            (test/expect (:stderr result) :to-contain "No config.toml "))))))
  
  (test/describe "Help"
    (test/it "shows help text"
      (let [result (cli-test/run ["--help"])]
        (test/expect (:exit-code result) :to-be 0)
        (test/expect (:stdout result) :to-contain "Usage")
        (test/expect (:stdout result) :to-contain "Commands")))))
"#;
        fs::write(path.join("tests/cli.test.ai"), test_content)?;

        // Create shell completion script
        let completion_script = format!(
            r#"#!/bin/bash
# Bash completion script for {}

_{}() {{
    local cur prev opts
    COMPREPLY=()
    cur="${{COMP_WORDS[COMP_CWORD]}}"
    prev="${{COMP_WORDS[COMP_CWORD-1]}}"
    
    # Commands
    local commands="init build run test clean"
    
    # Global options
    local global_opts="--help --verbose --config"
    
    case "${{prev}}" in
        init)
            COMPREPLY=( $(compgen -W "--template" -- "${{cur}}") )
            return 0
            ;;
        build)
            COMPREPLY=( $(compgen -W "--release" -- "${{cur}}") )
            return 0
            ;;
        test)
            COMPREPLY=( $(compgen -W "--filter" -- "${{cur}}") )
            return 0
            ;;
        --template|-t)
            COMPREPLY=( $(compgen -W "default lib" -- "${{cur}}") )
            return 0
            ;;
        --config|-c)
            COMPREPLY=( $(compgen -f -- "${{cur}}") )
            return 0
            ;;
    esac
    
    if [[ ${{COMP_CWORD}} -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "${{commands}} ${{global_opts}}" -- "${{cur}}") )
    fi
}}

complete -F _{} {}
"#,
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase()
        );
        fs::write(path.join("scripts/completion.bash"), completion_script)?;

        // Create installation script
        let install_script = format!(
            r#"#!/bin/bash
# Installation script for {}

set -e

echo "Installing {}..."

# Build the CLI
fluentai build -c Release

# Create installation directory
INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

# Copy binary
cp "target/release/{}" "$INSTALL_DIR/"
chmod +x "$INSTALL_DIR/{}"

# Install completions
if [ -d "$HOME/.bash_completion.d" ]; then
    cp scripts/completion.bash "$HOME/.bash_completion.d/{}.bash"
    echo "Bash completions installed"
fi

echo "Installation complete!"
echo "Add $INSTALL_DIR to your PATH if not already present:"
echo "  export PATH=\"$INSTALL_DIR:$PATH\""
"#,
            name,
            name,
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase()
        );
        fs::write(path.join("scripts/install.sh"), install_script)?;

        // Make scripts executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(path.join("scripts/install.sh"))?.permissions();
            perms.set_mode(0o755);
            fs::set_permissions(path.join("scripts/install.sh"), perms.clone())?;
            fs::set_permissions(path.join("scripts/completion.bash"), perms)?;
        }

        // Create README
        let readme_content = format!(
            r#"# {}

A command-line tool built with FluentAI.

## Installation

### From source

```bash
./scripts/install.sh
```

### Manual installation

```bash
fluentai build -c Release
cp target/release/{} /usr/local/bin/
```

## Usage

```bash
# Initialize a new project
{} init my-project

# Build the project
{} build
{} build --release

# Run the project
{} run
{} run -- arg1 arg2

# Run tests
{} test
{} test --filter pattern

# Clean build artifacts
{} clean
```

## Global Options

- `-v, --verbose` - Enable verbose output
- `-c, --config <FILE>` - Specify configuration file
- `--help` - Show help information

## Shell Completion

### Bash

```bash
source scripts/completion.bash
```

Add to your `.bashrc` for permanent completion support.

## Configuration

Projects use a `config.toml` file:

```toml
[project]
name = "my-project"
version = "1.0.0"
author = "Your Name"

[build]
output = "build/app"
optimize = true
```

## Development

### Running tests

```bash
fluentai test
```

### Building

```bash
fluentai build -c Release
```

## License

MIT
"#,
            name,
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase(),
            name.to_lowercase()
        );

        fs::write(path.join("README.md"), readme_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        Ok(())
    }
}
