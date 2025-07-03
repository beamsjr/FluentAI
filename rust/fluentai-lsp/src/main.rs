//! FluentAi Language Server executable

use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    fluentai_lsp::run_server().await
}