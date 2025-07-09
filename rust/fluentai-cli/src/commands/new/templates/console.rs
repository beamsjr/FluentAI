//! Console application template

use super::{helpers, Template, TemplateCategory, TemplateOptions};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct ConsoleTemplate;

impl Template for ConsoleTemplate {
    fn name(&self) -> &'static str {
        "console"
    }

    fn description(&self) -> &'static str {
        "Console application for command-line programs"
    }

    fn aliases(&self) -> Vec<&'static str> {
        vec!["app", "application"]
    }

    fn category(&self) -> TemplateCategory {
        TemplateCategory::Application
    }

    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // Create project file
        helpers::create_project_file(path, name, "Exe", &[])?;

        // Create main program file
        let program_content = r#";; FluentAI Console Application
;; Entry point for the application

(define main (args)
  (println "Hello, FluentAI!")
  0) ;; Return exit code

;; Run main if this is the entry module
(when (= __name__ "__main__")
  (exit (main (command-line-args))))
"#;
        fs::write(path.join("Program.ai"), program_content)?;

        // Create directories
        helpers::create_directories(path, &["src", "tests"])?;

        // Create a sample module
        let utils_content = r#";; Utility functions

(define greet (name)
  (format "Hello, {}!" name))

(export greet)
"#;
        fs::write(path.join("src/Utils.ai"), utils_content)?;

        // Create a sample test
        let test_content = r#";; Tests for Utils module

(import "../src/Utils.ai" :as utils)
(import "fluentai/test" :as test)

(test/describe "Utils"
  (test/it "greets correctly"
    (test/expect (utils/greet "World") 
                 :to-equal "Hello, World!")))
"#;
        fs::write(path.join("tests/Utils.test.ai"), test_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        // Create README
        helpers::create_readme(path, name, "A FluentAI console application.")?;

        Ok(())
    }
}
