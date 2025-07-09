//! Similarity computation utilities

use anyhow::Result;

/// Compute cosine similarity between two embeddings
pub fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
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

/// Compute Euclidean distance between two embeddings
pub fn euclidean_distance(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return f32::INFINITY;
    }

    a.iter()
        .zip(b.iter())
        .map(|(x, y)| (x - y).powi(2))
        .sum::<f32>()
        .sqrt()
}

/// Compute Manhattan distance between two embeddings
pub fn manhattan_distance(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return f32::INFINITY;
    }

    a.iter().zip(b.iter()).map(|(x, y)| (x - y).abs()).sum()
}

/// Find k-nearest neighbors using brute force search
pub fn knn_search(
    query: &[f32],
    embeddings: &[(usize, Vec<f32>)],
    k: usize,
    metric: SimilarityMetric,
) -> Vec<(usize, f32)> {
    let mut scores: Vec<(usize, f32)> = embeddings
        .iter()
        .map(|(id, emb)| {
            let score = match metric {
                SimilarityMetric::Cosine => cosine_similarity(query, emb),
                SimilarityMetric::Euclidean => -euclidean_distance(query, emb),
                SimilarityMetric::Manhattan => -manhattan_distance(query, emb),
            };
            (*id, score)
        })
        .collect();

    // Sort by score (descending)
    scores.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    scores.truncate(k);

    scores
}

/// Similarity metric to use
#[derive(Debug, Clone, Copy)]
pub enum SimilarityMetric {
    Cosine,
    Euclidean,
    Manhattan,
}

/// Cluster embeddings using k-means
pub fn kmeans_cluster(
    embeddings: &[Vec<f32>],
    k: usize,
    max_iterations: usize,
) -> Result<Vec<usize>> {
    if embeddings.is_empty() || k == 0 {
        return Ok(vec![]);
    }

    let n = embeddings.len();
    let dim = embeddings[0].len();

    // Initialize centroids randomly
    let mut centroids: Vec<Vec<f32>> = (0..k).map(|i| embeddings[i % n].clone()).collect();

    let mut assignments = vec![0; n];

    for _ in 0..max_iterations {
        let mut changed = false;

        // Assign points to nearest centroid
        for (i, emb) in embeddings.iter().enumerate() {
            let mut min_dist = f32::INFINITY;
            let mut best_cluster = 0;

            for (j, centroid) in centroids.iter().enumerate() {
                let dist = euclidean_distance(emb, centroid);
                if dist < min_dist {
                    min_dist = dist;
                    best_cluster = j;
                }
            }

            if assignments[i] != best_cluster {
                assignments[i] = best_cluster;
                changed = true;
            }
        }

        if !changed {
            break;
        }

        // Update centroids
        for (j, centroid) in centroids.iter_mut().enumerate() {
            let cluster_points: Vec<&Vec<f32>> = embeddings
                .iter()
                .enumerate()
                .filter(|(i, _)| assignments[*i] == j)
                .map(|(_, emb)| emb)
                .collect();

            if !cluster_points.is_empty() {
                // Compute mean
                for d in 0..dim {
                    centroid[d] = cluster_points.iter().map(|emb| emb[d]).sum::<f32>()
                        / cluster_points.len() as f32;
                }
            }
        }
    }

    Ok(assignments)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cosine_similarity() {
        let a = vec![1.0, 0.0, 0.0];
        let b = vec![0.0, 1.0, 0.0];
        let c = vec![1.0, 0.0, 0.0];

        assert_eq!(cosine_similarity(&a, &b), 0.0);
        assert_eq!(cosine_similarity(&a, &c), 1.0);
    }

    #[test]
    fn test_euclidean_distance() {
        let a = vec![0.0, 0.0];
        let b = vec![3.0, 4.0];

        assert_eq!(euclidean_distance(&a, &b), 5.0);
    }

    #[test]
    fn test_knn_search() {
        let embeddings = vec![
            (0, vec![1.0, 0.0]),
            (1, vec![0.0, 1.0]),
            (2, vec![0.5, 0.5]),
        ];

        let query = vec![0.8, 0.2];
        let results = knn_search(&query, &embeddings, 2, SimilarityMetric::Cosine);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, 0); // Closest to [1.0, 0.0]
    }
}
