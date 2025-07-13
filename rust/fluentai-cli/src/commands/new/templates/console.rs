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
        let program_content = r#"// main.flc
// FluentAI Console Application

private function main() {
    $("Hello, FluentAI!").print();
}

// Run the application
main()
"#;
        fs::write(path.join("main.flc"), program_content)?;

        // Create directories
        helpers::create_directories(path, &["src", "tests"])?;

        // Create a sample module
        let utils_content = r#"// Utils.flc
module Utils;

public function greet(name: string) -> string {
    f"Hello, {name}!"
}
"#;
        fs::write(path.join("src/Utils.flc"), utils_content)?;

        // Create a sample test
        let test_content = r#"// Utils.test.flc
use ../src/Utils::{greet};
use std::test::{describe, it, expect};

describe("Utils", () => {
    it("greets correctly", () => {
        expect(greet("World")).to_equal("Hello, World!");
    });
});
"#;
        fs::write(path.join("tests/Utils.test.flc"), test_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        // Create README
        helpers::create_readme(path, name, "A FluentAI console application.")?;

        Ok(())
    }
}
