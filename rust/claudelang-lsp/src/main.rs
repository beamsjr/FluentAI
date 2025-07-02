//! ClaudeLang Language Server executable

use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    claudelang_lsp::run_server().await
}