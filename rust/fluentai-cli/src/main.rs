//! FluentAi CLI - Main interpreter and tools

#![warn(missing_docs)]

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod commands;
mod config;
mod runner;

use commands::new::templates;
use commands::{build, new, package, publish, repl, restore, run, test};

#[derive(Parser)]
#[command(name = "fluentai")]
#[command(about = "FluentAi interpreter and tools", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Path to configuration file
    #[arg(long, global = true)]
    config: Option<PathBuf>,

    /// Enable debug output
    #[arg(long, global = true)]
    debug: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new FluentAI project
    New {
        /// Project template (console, library, webservice)
        template: String,

        /// Project name
        name: String,

        /// Output directory (defaults to project name)
        #[arg(long)]
        output: Option<PathBuf>,

        /// Authentication type (jwt, oauth, basic, session)
        #[arg(long)]
        auth: Option<String>,

        /// Database type (postgres, mysql, sqlite, mongodb)
        #[arg(long)]
        database: Option<String>,

        /// Frontend framework (htmx, alpine, vue, react)
        #[arg(long)]
        frontend: Option<String>,

        /// Queue type for worker template (redis, rabbitmq, sqs)
        #[arg(long)]
        queue: Option<String>,

        /// Include scheduler for worker template
        #[arg(long)]
        scheduler: bool,

        /// Include Docker support
        #[arg(long)]
        docker: bool,

        /// Include CI/CD configuration
        #[arg(long)]
        ci: bool,
    },

    /// Build a FluentAI project
    Build {
        /// Project path (defaults to current directory)
        #[arg(long)]
        project: Option<PathBuf>,

        /// Build configuration (Debug or Release)
        #[arg(short, long, default_value = "Debug")]
        configuration: String,

        /// Output path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Run a FluentAi program
    Run {
        /// Path to the FluentAi file
        file: PathBuf,

        /// Optimization level (0=none, 1=basic, 2=standard, 3=aggressive)
        #[arg(short = 'O', long = "opt", default_value = "2")]
        optimization: u8,

        /// Enable visualization
        #[cfg(feature = "visualization")]
        #[arg(long, short = 'v')]
        visualize: bool,

        /// Visualization server port
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "8080")]
        viz_port: u16,

        /// Delay between execution steps (ms)
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "0")]
        viz_delay: u64,

        /// Automatically open browser
        #[cfg(feature = "visualization")]
        #[arg(long)]
        viz_open: bool,

        /// Program arguments
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },

    /// Run tests
    Test {
        /// Project path (defaults to current directory)
        #[arg(long)]
        project: Option<PathBuf>,

        /// Filter tests by name
        #[arg(long)]
        filter: Option<String>,

        /// Run tests in parallel
        #[arg(long)]
        parallel: bool,

        /// Generate coverage report
        #[arg(long)]
        coverage: bool,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Publish a FluentAI application
    Publish {
        /// Project path (defaults to current directory)
        #[arg(long)]
        project: Option<PathBuf>,

        /// Build configuration (Debug or Release)
        #[arg(short, long, default_value = "Release")]
        configuration: String,

        /// Runtime option (framework-dependent or self-contained)
        #[arg(long, default_value = "framework-dependent")]
        runtime: String,

        /// Target platform (win-x64, linux-x64, osx-x64, osx-arm64, wasm)
        #[arg(short = 'r', long)]
        target: Option<String>,

        /// Output path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Create single-file executable
        #[arg(long)]
        single_file: bool,

        /// Trim unused code
        #[arg(long)]
        trimmed: bool,
    },

    /// Restore project dependencies
    Restore {
        /// Project path (defaults to current directory)
        #[arg(long)]
        project: Option<PathBuf>,

        /// Force restore even if up to date
        #[arg(long)]
        force: bool,
    },

    /// Start interactive REPL
    Repl {
        /// Enable visualization
        #[cfg(feature = "visualization")]
        #[arg(long, short = 'v')]
        visualize: bool,

        /// Visualization server port
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "8080")]
        viz_port: u16,
    },

    /// Package management commands
    #[command(subcommand)]
    Package(PackageCommands),

    /// Add a package reference
    Add {
        /// Package name
        package: String,

        /// Package version
        #[arg(long)]
        version: Option<String>,

        /// Project path (defaults to current directory)
        #[arg(long)]
        project: Option<PathBuf>,
    },

    /// Run with visualization (shortcut for run --visualize)
    #[cfg(feature = "visualization")]
    Viz {
        /// Path to the FluentAi file
        file: PathBuf,

        /// Server port
        #[arg(long, default_value = "8080")]
        port: u16,

        /// Automatically open browser
        #[arg(long)]
        open: bool,

        /// Program arguments
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}

#[derive(Subcommand)]
enum PackageCommands {
    /// Initialize a new package
    Init {
        /// Package name
        #[arg(long)]
        name: Option<String>,
    },

    /// Install dependencies
    Install {
        /// Include dev dependencies
        #[arg(long)]
        dev: bool,
    },

    /// Add a dependency
    Add {
        /// Package name
        package: String,

        /// Version requirement
        #[arg(long)]
        version: Option<String>,

        /// Add as dev dependency
        #[arg(long)]
        dev: bool,
    },

    /// Remove a dependency
    Remove {
        /// Package name
        package: String,
    },

    /// Update dependencies
    Update {
        /// Specific packages to update
        packages: Vec<String>,
    },

    /// List installed packages
    List {
        /// Show as tree
        #[arg(long)]
        tree: bool,
    },

    /// Search for packages
    Search {
        /// Search query
        query: String,

        /// Maximum results
        #[arg(long, default_value = "20")]
        limit: usize,
    },

    /// Publish a package
    Publish {
        /// Authentication token
        #[arg(long, env = "FLUENTAI_TOKEN")]
        token: String,
    },

    /// Run a package script
    Run {
        /// Script name
        script: String,

        /// Script arguments
        args: Vec<String>,
    },

    /// Clean unused packages
    Clean,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    tracing_subscriber::fmt::init();

    // Load configuration
    let config = config::load_config(cli.config)?;

    // Handle commands
    match cli.command {
        Some(Commands::New {
            template,
            name,
            output,
            auth,
            database,
            frontend,
            queue,
            scheduler,
            docker,
            ci,
        }) => {
            let mut options = templates::TemplateOptions {
                auth,
                database,
                frontend,
                docker,
                ci,
                custom: std::collections::HashMap::new(),
            };

            // Add custom options for specific templates
            if let Some(queue) = queue {
                options.custom.insert("queue".to_string(), queue);
            }
            if scheduler {
                options
                    .custom
                    .insert("scheduler".to_string(), "true".to_string());
            }

            new::new_project(&template, &name, output, options).await?;
        }

        Some(Commands::Build {
            project,
            configuration,
            output,
            verbose,
        }) => {
            let build_config = build::BuildConfig {
                configuration,
                output_path: output,
                target: build::BuildTarget::Executable,
                optimization_level: 2,
                verbose,
            };
            build::build(project, build_config).await?;
        }

        Some(Commands::Run {
            file,
            optimization,
            #[cfg(feature = "visualization")]
            visualize,
            #[cfg(feature = "visualization")]
            viz_port,
            #[cfg(feature = "visualization")]
            viz_delay,
            #[cfg(feature = "visualization")]
            viz_open,
            args,
        }) => {
            #[cfg(feature = "visualization")]
            let viz_config = if visualize {
                Some(run::VisualizationConfig {
                    port: viz_port,
                    delay_ms: viz_delay,
                    auto_open: viz_open,
                })
            } else {
                None
            };

            #[cfg(not(feature = "visualization"))]
            let viz_config = None;

            run::run_file(&file, args, viz_config, optimization, &config).await?;
        }

        #[cfg(feature = "visualization")]
        Some(Commands::Viz {
            file,
            port,
            open,
            args,
        }) => {
            let viz_config = Some(run::VisualizationConfig {
                port,
                delay_ms: 0,
                auto_open: open,
            });
            run::run_file(&file, args, viz_config, 2, &config).await?; // Default to standard optimization
        }

        Some(Commands::Test {
            project,
            filter,
            parallel,
            coverage,
            verbose,
        }) => {
            let test_config = test::TestConfig {
                filter,
                verbose,
                parallel,
                coverage,
            };
            test::test(project, test_config).await?;
        }

        Some(Commands::Publish {
            project,
            configuration,
            runtime,
            target,
            output,
            single_file,
            trimmed,
        }) => {
            let runtime_opt = match runtime.as_str() {
                "self-contained" => publish::RuntimeOption::SelfContained,
                _ => publish::RuntimeOption::FrameworkDependent,
            };

            let publish_target = target
                .and_then(|t| publish::PublishTarget::from_str(&t))
                .unwrap_or(publish::PublishTarget::Current);

            let publish_config = publish::PublishConfig {
                configuration,
                runtime: runtime_opt,
                target: publish_target,
                output_path: output,
                single_file,
                trimmed,
            };
            publish::publish(project, publish_config).await?;
        }

        Some(Commands::Restore { project, force }) => {
            restore::restore(project, force).await?;
        }

        Some(Commands::Repl {
            #[cfg(feature = "visualization")]
            visualize,
            #[cfg(feature = "visualization")]
            viz_port,
        }) => {
            #[cfg(feature = "visualization")]
            let viz_config = if visualize {
                Some(repl::VisualizationConfig { port: viz_port })
            } else {
                None
            };

            #[cfg(not(feature = "visualization"))]
            let viz_config = None;

            repl::start_repl(viz_config, &config).await?;
        }

        Some(Commands::Package(cmd)) => {
            package::handle_package_command(cmd, &config).await?;
        }

        Some(Commands::Add {
            package: pkg,
            version,
            project,
        }) => {
            // Add package is a convenience command that modifies the project file
            // and runs restore
            println!("Adding package {} to project...", pkg);
            // TODO: Implement project file modification
            restore::restore(project, false).await?;
        }

        None => {
            // No command specified, start REPL
            repl::start_repl(None, &config).await?;
        }
    }

    Ok(())
}
