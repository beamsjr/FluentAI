//! Example demonstrating embedding generation from documentation

use fluentai_embeddings::doc_embeddings::DocumentationEmbeddingService;
use fluentai_embeddings::similarity::{cosine_similarity, knn_search, SimilarityMetric};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("=== Documentation Embedding Generation ===\n");
    
    // Create the documentation embedding service
    let mut service = DocumentationEmbeddingService::new().await?;
    
    // Generate embeddings for all documentation
    println!("Generating embeddings for all documentation...");
    let embeddings = service.generate_all_embeddings().await?;
    
    println!("Generated {} embeddings\n", embeddings.len());
    
    // Show some examples
    println!("Sample embeddings generated:");
    for (key, _) in embeddings.iter().take(10) {
        println!("  - {}", key);
    }
    
    // Demonstrate similarity search
    println!("\n=== Similarity Search Demo ===\n");
    
    // Get the embedding service to access embeddings
    let embedding_service = &service.embedding_service;
    
    // Get embeddings for some specific items
    if let (Some(&if_embedding), Some(&lambda_embedding), Some(&let_embedding)) = (
        embeddings.get("kw_if"),
        embeddings.get("kw_lambda"),
        embeddings.get("kw_let"),
    ) {
        // Retrieve the actual embedding vectors
        let if_vec = embedding_service.get_embedding(if_embedding).await?;
        let lambda_vec = embedding_service.get_embedding(lambda_embedding).await?;
        let let_vec = embedding_service.get_embedding(let_embedding).await?;
        
        // Calculate similarities
        let if_lambda_sim = cosine_similarity(&if_vec, &lambda_vec);
        let if_let_sim = cosine_similarity(&if_vec, &let_vec);
        let lambda_let_sim = cosine_similarity(&lambda_vec, &let_vec);
        
        println!("Cosine similarities between keywords:");
        println!("  if <-> lambda: {:.3}", if_lambda_sim);
        println!("  if <-> let: {:.3}", if_let_sim);
        println!("  lambda <-> let: {:.3}", lambda_let_sim);
    }
    
    // Find similar documentation items
    println!("\n=== Finding Similar Documentation ===\n");
    
    if let Some(&map_embedding) = embeddings.get("builtin_list_map") {
        let map_vec = embedding_service.get_embedding(map_embedding).await?;
        
        // Create a list of all embeddings for search
        let mut all_embeddings = Vec::new();
        for (name, &id) in &embeddings {
            if name != "builtin_list_map" {
                let vec = embedding_service.get_embedding(id).await?;
                all_embeddings.push((id.0, vec));
            }
        }
        
        // Find 5 most similar items
        let similar = knn_search(&map_vec, &all_embeddings, 5, SimilarityMetric::Cosine);
        
        println!("Items most similar to 'map':");
        for (id, score) in similar {
            // Find the name for this ID
            for (name, &embedding_id) in &embeddings {
                if embedding_id.0 == id {
                    println!("  - {} (similarity: {:.3})", name, score);
                    break;
                }
            }
        }
    }
    
    // Demonstrate clustering of documentation
    println!("\n=== Documentation Categories ===\n");
    
    // Group by category prefixes
    let mut categories: std::collections::HashMap<&str, Vec<&str>> = std::collections::HashMap::new();
    
    for key in embeddings.keys() {
        let category = if key.starts_with("kw_") {
            "Keywords"
        } else if key.starts_with("op_") {
            "Operators"
        } else if key.starts_with("builtin_math") {
            "Math Functions"
        } else if key.starts_with("builtin_list") {
            "List Functions"
        } else if key.starts_with("builtin_string") {
            "String Functions"
        } else {
            "Other"
        };
        
        categories.entry(category).or_default().push(key);
    }
    
    for (category, items) in categories {
        println!("{}: {} items", category, items.len());
        for item in items.iter().take(3) {
            println!("  - {}", item);
        }
        if items.len() > 3 {
            println!("  ... and {} more", items.len() - 3);
        }
    }
    
    Ok(())
}