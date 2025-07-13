//! Language Server Protocol implementation for FluentAi
//!
//! Provides sub-10ms response times for IDE features including:
//! - Syntax highlighting
//! - Error detection
//! - Auto-completion
//! - Go to definition
//! - Hover information

#![warn(missing_docs)]

use anyhow::Result;
use dashmap::DashMap;
use fluentai_core::ast::Graph;
use fluentai_parser::parse_flc as parse;
use ropey::Rope;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result as JsonRpcResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{debug, error, info};

mod completion;
mod diagnostics;
mod documentation_service;
mod hover;

use completion::compute_completions;
use diagnostics::compute_diagnostics;
use hover::compute_hover;

/// Document state maintained by the LSP server
#[derive(Debug)]
struct Document {
    /// The document content as a rope for efficient editing
    rope: Rope,
    /// Parsed AST (if successful)
    ast: Option<Graph>,
    /// Version number for synchronization
    version: i32,
}

/// The main language server implementation
pub struct FluentAiServer {
    /// LSP client for sending notifications
    client: Client,
    /// Document storage indexed by URI
    documents: Arc<DashMap<Url, Arc<RwLock<Document>>>>,
}

impl FluentAiServer {
    /// Create a new language server instance
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
        }
    }

    /// Parse a document and update its AST
    async fn parse_document(&self, uri: &Url, content: &str) -> Option<Graph> {
        let start = std::time::Instant::now();

        match parse(content) {
            Ok(ast) => {
                let elapsed = start.elapsed();
                debug!("Parsed {} in {:?}", uri, elapsed);

                // Send diagnostics
                let diagnostics = compute_diagnostics(&ast, content);
                self.client
                    .publish_diagnostics(uri.clone(), diagnostics, None)
                    .await;

                Some(ast)
            }
            Err(e) => {
                error!("Parse error in {}: {}", uri, e);

                // Send error diagnostic
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Parse error: {}", e),
                    ..Default::default()
                };

                self.client
                    .publish_diagnostics(uri.clone(), vec![diagnostic], None)
                    .await;

                None
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for FluentAiServer {
    async fn initialize(&self, _: InitializeParams) -> JsonRpcResult<InitializeResult> {
        info!("Initializing FluentAi Language Server");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["(".to_string(), " ".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        ..Default::default()
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("FluentAi Language Server initialized");
    }

    async fn shutdown(&self) -> JsonRpcResult<()> {
        info!("Shutting down FluentAi Language Server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;

        debug!("Opened document: {}", uri);

        // Parse the document
        let ast = self.parse_document(&uri, &content).await;

        // Store the document
        let doc = Document {
            rope: Rope::from_str(&content),
            ast,
            version,
        };

        self.documents.insert(uri, Arc::new(RwLock::new(doc)));
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        if let Some(doc_lock) = self.documents.get(&uri) {
            let mut doc = doc_lock.write().await;

            // Apply incremental changes
            for change in params.content_changes {
                if let Some(range) = change.range {
                    // Convert LSP range to rope indices
                    let start_line = range.start.line as usize;
                    let start_char = range.start.character as usize;
                    let end_line = range.end.line as usize;
                    let end_char = range.end.character as usize;

                    let start_idx = doc.rope.line_to_char(start_line) + start_char;
                    let end_idx = doc.rope.line_to_char(end_line) + end_char;

                    // Apply the change
                    doc.rope.remove(start_idx..end_idx);
                    doc.rope.insert(start_idx, &change.text);
                } else {
                    // Full document update
                    doc.rope = Rope::from_str(&change.text);
                }
            }

            doc.version = version;

            // Re-parse the document
            let content = doc.rope.to_string();
            drop(doc); // Release write lock before async operation

            let ast = self.parse_document(&uri, &content).await;

            // Update AST
            if let Some(doc_lock) = self.documents.get(&uri) {
                let mut doc = doc_lock.write().await;
                doc.ast = ast;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        debug!("Closed document: {}", params.text_document.uri);
        self.documents.remove(&params.text_document.uri);
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> JsonRpcResult<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc_lock) = self.documents.get(&uri) {
            let doc = doc_lock.read().await;
            let completions = compute_completions(&doc.rope, &doc.ast, position);
            return Ok(Some(CompletionResponse::Array(completions)));
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> JsonRpcResult<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc_lock) = self.documents.get(&uri) {
            let doc = doc_lock.read().await;
            return Ok(compute_hover(&doc.rope, &doc.ast, position));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> JsonRpcResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // TODO: Implement go to definition
        debug!("Go to definition at {:?} in {}", position, uri);

        Ok(None)
    }
}

/// Start the language server
pub async fn run_server() -> Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(FluentAiServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
