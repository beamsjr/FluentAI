//! Generate embeddings from documentation

use crate::{EmbeddingService, EmbeddingId};
use fluentai_core::documentation::{Documentation, DocumentationRegistry, OperatorDoc, KeywordDoc, BuiltinDoc};
use anyhow::Result;
use std::collections::HashMap;

/// Service for generating embeddings from documentation
pub struct DocumentationEmbeddingService {
    embedding_service: EmbeddingService,
    registry: DocumentationRegistry,
}

impl DocumentationEmbeddingService {
    /// Create a new documentation embedding service
    pub async fn new() -> Result<Self> {
        Ok(Self {
            embedding_service: EmbeddingService::default()?,
            registry: DocumentationRegistry::new(),
        })
    }
    
    /// Generate embeddings for all documentation in the registry
    pub async fn generate_all_embeddings(&mut self) -> Result<HashMap<String, EmbeddingId>> {
        let mut embeddings = HashMap::new();
        
        // Generate embeddings for all documentation
        let docs: Vec<_> = self.registry.list_all().to_vec();
        for doc in docs {
            let embedding_id = self.generate_doc_embedding(&doc.name, &doc).await?;
            embeddings.insert(doc.name.clone(), embedding_id);
        }
        
        // Generate embeddings for operators
        let operators: Vec<_> = self.registry.get_operators().to_vec();
        for op in operators {
            let key = format!("op_{}", op.symbol);
            let embedding_id = self.generate_operator_embedding(&op).await?;
            embeddings.insert(key, embedding_id);
        }
        
        // Generate embeddings for keywords
        let keywords: Vec<_> = self.registry.get_keywords().to_vec();
        for kw in keywords {
            let key = format!("kw_{}", kw.keyword);
            let embedding_id = self.generate_keyword_embedding(&kw).await?;
            embeddings.insert(key, embedding_id);
        }
        
        // Generate embeddings for built-ins
        let builtins: Vec<_> = self.registry.get_builtins().to_vec();
        for builtin in builtins {
            let key = format!("builtin_{}_{}", builtin.module, builtin.name);
            let embedding_id = self.generate_builtin_embedding(&builtin).await?;
            embeddings.insert(key, embedding_id);
        }
        
        Ok(embeddings)
    }
    
    /// Generate embedding for a documentation entry
    async fn generate_doc_embedding(&mut self, name: &str, doc: &Documentation) -> Result<EmbeddingId> {
        let features = self.extract_doc_features(name, doc);
        let embedding = self.generate_embedding_from_features(features)?;
        self.embedding_service.store_embedding(embedding).await
    }
    
    /// Generate embedding for an operator
    async fn generate_operator_embedding(&mut self, op: &OperatorDoc) -> Result<EmbeddingId> {
        let features = self.extract_operator_features(op);
        let embedding = self.generate_embedding_from_features(features)?;
        self.embedding_service.store_embedding(embedding).await
    }
    
    /// Generate embedding for a keyword
    async fn generate_keyword_embedding(&mut self, kw: &KeywordDoc) -> Result<EmbeddingId> {
        let features = self.extract_keyword_features(kw);
        let embedding = self.generate_embedding_from_features(features)?;
        self.embedding_service.store_embedding(embedding).await
    }
    
    /// Generate embedding for a built-in function
    async fn generate_builtin_embedding(&mut self, builtin: &BuiltinDoc) -> Result<EmbeddingId> {
        let features = self.extract_builtin_features(builtin);
        let embedding = self.generate_embedding_from_features(features)?;
        self.embedding_service.store_embedding(embedding).await
    }
    
    /// Extract features from documentation
    fn extract_doc_features(&self, name: &str, doc: &Documentation) -> HashMap<String, f32> {
        let mut features = HashMap::new();
        
        // Name features
        self.add_text_features(&mut features, "name", name);
        
        // Description features
        self.add_text_features(&mut features, "desc", &doc.description);
        
        // Syntax features
        if !doc.syntax.is_empty() {
            self.add_text_features(&mut features, "syntax", &doc.syntax);
        }
        
        // Category features
        self.add_category_enum_features(&mut features, &doc.category);
        
        // Example count as a feature
        features.insert("example_count".to_string(), doc.examples.len() as f32);
        
        // See also relationships
        features.insert("see_also_count".to_string(), doc.see_also.len() as f32);
        
        features
    }
    
    /// Extract features from operator documentation
    fn extract_operator_features(&self, op: &OperatorDoc) -> HashMap<String, f32> {
        let mut features = HashMap::new();
        
        // Operator characteristics
        features.insert("op_precedence".to_string(), op.precedence as f32);
        features.insert("op_is_binary".to_string(), 1.0); // Most operators are binary
        
        // Associativity
        match op.associativity {
            fluentai_core::documentation::Associativity::Left => {
                features.insert("op_assoc_left".to_string(), 1.0);
            }
            fluentai_core::documentation::Associativity::Right => {
                features.insert("op_assoc_right".to_string(), 1.0);
            }
            fluentai_core::documentation::Associativity::None => {
                features.insert("op_assoc_none".to_string(), 1.0);
            }
        }
        
        // Text features
        self.add_text_features(&mut features, "op_name", &op.name);
        self.add_text_features(&mut features, "op_desc", &op.description);
        
        features
    }
    
    /// Extract features from keyword documentation
    fn extract_keyword_features(&self, kw: &KeywordDoc) -> HashMap<String, f32> {
        let mut features = HashMap::new();
        
        // Keyword type features
        features.insert("kw_control_flow".to_string(), 
            if kw.keyword == "if" || kw.keyword == "cond" || kw.keyword == "match" { 1.0 } else { 0.0 });
        features.insert("kw_binding".to_string(),
            if kw.keyword == "let" || kw.keyword == "letrec" || kw.keyword == "define" { 1.0 } else { 0.0 });
        features.insert("kw_function".to_string(),
            if kw.keyword == "lambda" || kw.keyword == "fn" { 1.0 } else { 0.0 });
        
        // Text features
        self.add_text_features(&mut features, "kw_name", &kw.keyword);
        self.add_text_features(&mut features, "kw_desc", &kw.description);
        self.add_text_features(&mut features, "kw_syntax", &kw.syntax);
        
        features
    }
    
    /// Extract features from built-in documentation
    fn extract_builtin_features(&self, builtin: &BuiltinDoc) -> HashMap<String, f32> {
        let mut features = HashMap::new();
        
        // Module features
        self.add_text_features(&mut features, "builtin_module", &builtin.module);
        
        // Function characteristics from signature
        let arg_count = builtin.signature.matches("->").count() as f32;
        features.insert("builtin_arity".to_string(), arg_count);
        
        // Category features based on module
        match builtin.module {
            "math" => features.insert("cat_numeric".to_string(), 1.0),
            "string" => features.insert("cat_string".to_string(), 1.0),
            "list" => features.insert("cat_collection".to_string(), 1.0),
            "io" => features.insert("cat_io".to_string(), 1.0),
            _ => features.insert("cat_other".to_string(), 1.0),
        };
        
        // Text features
        self.add_text_features(&mut features, "builtin_name", &builtin.name);
        self.add_text_features(&mut features, "builtin_desc", &builtin.description);
        
        features
    }
    
    /// Add text-based features using simple heuristics
    fn add_text_features(&self, features: &mut HashMap<String, f32>, prefix: &str, text: &str) {
        // Word count
        let word_count = text.split_whitespace().count() as f32;
        features.insert(format!("{}_word_count", prefix), word_count);
        
        // Character length (normalized)
        let char_len = (text.len() as f32).min(1000.0) / 1000.0;
        features.insert(format!("{}_length", prefix), char_len);
        
        // Common programming keywords
        let keywords = ["function", "return", "value", "list", "string", "number", 
                       "error", "effect", "async", "pure", "side", "mutation"];
        
        for keyword in &keywords {
            if text.to_lowercase().contains(keyword) {
                features.insert(format!("{}_{}", prefix, keyword), 1.0);
            }
        }
    }
    
    /// Generate embedding from features using the default generator
    fn generate_embedding_from_features(&self, features: HashMap<String, f32>) -> Result<Vec<f32>> {
        // Use the FeatureBasedGenerator directly
        let generator = crate::generator::FeatureBasedGenerator::new();
        generator.generate_from_features(&features)
    }
    
    /// Add category-based features from enum
    fn add_category_enum_features(&self, features: &mut HashMap<String, f32>, category: &fluentai_core::documentation::DocumentationCategory) {
        use fluentai_core::documentation::DocumentationCategory;
        
        let cat_str = match category {
            DocumentationCategory::Literal => "literal",
            DocumentationCategory::Variable => "variable",
            DocumentationCategory::Function => "function",
            DocumentationCategory::ControlFlow => "control_flow",
            DocumentationCategory::DataStructure => "data_structure",
            DocumentationCategory::PatternMatching => "pattern_matching",
            DocumentationCategory::Module => "module",
            DocumentationCategory::Async => "async",
            DocumentationCategory::Effect => "effect",
            DocumentationCategory::Operator => "operator",
            DocumentationCategory::Keyword => "keyword",
            DocumentationCategory::Verification => "verification",
        };
        
        features.insert(format!("cat_{}", cat_str), 1.0);
    }
    
    /// Add category-based features
    #[allow(dead_code)]
    fn add_category_features(&self, features: &mut HashMap<String, f32>, category: &str) {
        let categories = [
            "core", "control-flow", "data-structures", "functions",
            "operators", "io", "math", "string", "list", "effects"
        ];
        
        for cat in &categories {
            if category.to_lowercase().contains(cat) {
                features.insert(format!("cat_{}", cat.replace('-', "_")), 1.0);
            }
        }
    }
    
    /// Search documentation by semantic similarity
    pub async fn search_by_similarity(
        &self, 
        query: &str, 
        limit: usize,
        threshold: f32
    ) -> Result<Vec<(String, Documentation, f32)>> {
        // Generate embedding for the query
        let mut query_features = HashMap::new();
        self.add_text_features(&mut query_features, "query", query);
        
        let query_embedding = self.generate_embedding_from_features(query_features)?;
        let query_id = self.embedding_service.store_embedding(query_embedding).await?;
        
        // Find similar embeddings
        let similar = self.embedding_service.find_similar(query_id, limit * 2, threshold).await?;
        
        // Map back to documentation
        let results = Vec::new();
        for (embedding_id, score) in similar {
            // This would need a reverse mapping from embedding_id to doc name
            // For now, we'll skip this implementation detail
            // In a real system, we'd maintain a bidirectional mapping
        }
        
        Ok(results)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_generate_doc_embeddings() {
        let mut service = DocumentationEmbeddingService::new().await.unwrap();
        let embeddings = service.generate_all_embeddings().await.unwrap();
        
        // Should have embeddings for all documentation
        assert!(!embeddings.is_empty());
        
        // Check that core functions have embeddings
        assert!(embeddings.contains_key("kw_if"));
        assert!(embeddings.contains_key("kw_lambda"));
        assert!(embeddings.contains_key("op_+"));
    }
}