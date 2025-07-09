fn create_normalized_embedding(values: &[f32]) -> Vec<f32> {
    let norm: f32 = values.iter().map(|x| x * x).sum::<f32>().sqrt();
    if norm > 0.0 {
        values.iter().map(|x| x / norm).collect()
    } else {
        values.to_vec()
    }
}

fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return 0.0;
    }

    let dot: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let norm_a: f32 = a.iter().map(|x| x * x).sum::<f32>().sqrt();
    let norm_b: f32 = b.iter().map(|x| x * x).sum::<f32>().sqrt();

    if norm_a > 0.0 && norm_b > 0.0 {
        dot / (norm_a * norm_b)
    } else {
        0.0
    }
}

fn main() {
    let emb1 = create_normalized_embedding(&[1.0, 0.0, 0.0]);
    let emb2 = create_normalized_embedding(&[0.9, 0.1, 0.0]);

    println!("emb1: {:?}", emb1);
    println!("emb2: {:?}", emb2);

    let sim = cosine_similarity(&emb1, &emb2);
    println!("Cosine similarity: {}", sim);

    // Check if it's >= 0.95
    println!("Is >= 0.95? {}", sim >= 0.95);
}
