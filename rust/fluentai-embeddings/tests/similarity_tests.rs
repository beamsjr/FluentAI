//! Comprehensive tests for similarity functions

use fluentai_embeddings::similarity::*;
use proptest::prelude::*;

// Helper to create normalized vectors
fn normalize(v: &[f32]) -> Vec<f32> {
    let norm: f32 = v.iter().map(|x| x * x).sum::<f32>().sqrt();
    if norm > 0.0 {
        v.iter().map(|x| x / norm).collect()
    } else {
        v.to_vec()
    }
}

#[test]
fn test_cosine_similarity_basic() {
    // Orthogonal vectors
    assert_eq!(cosine_similarity(&[1.0, 0.0], &[0.0, 1.0]), 0.0);

    // Identical vectors
    assert_eq!(cosine_similarity(&[1.0, 0.0], &[1.0, 0.0]), 1.0);

    // Opposite vectors
    assert_eq!(cosine_similarity(&[1.0, 0.0], &[-1.0, 0.0]), -1.0);

    // 45 degree angle
    let v1 = normalize(&[1.0, 0.0]);
    let v2 = normalize(&[1.0, 1.0]);
    let sim = cosine_similarity(&v1, &v2);
    assert!((sim - 0.707).abs() < 0.01);
}

#[test]
fn test_cosine_similarity_edge_cases() {
    // Empty vectors
    assert_eq!(cosine_similarity(&[], &[]), 0.0);

    // Different lengths
    assert_eq!(cosine_similarity(&[1.0, 2.0], &[1.0, 2.0, 3.0]), 0.0);

    // Zero vectors
    assert_eq!(cosine_similarity(&[0.0, 0.0, 0.0], &[1.0, 2.0, 3.0]), 0.0);
    assert_eq!(cosine_similarity(&[1.0, 2.0, 3.0], &[0.0, 0.0, 0.0]), 0.0);
    assert_eq!(cosine_similarity(&[0.0, 0.0], &[0.0, 0.0]), 0.0);

    // Very small values
    let tiny = f32::EPSILON;
    assert!(cosine_similarity(&[tiny, tiny], &[tiny, tiny]) > 0.99);

    // Very large values
    let large = 1e10;
    assert!((cosine_similarity(&[large, 0.0], &[large, 0.0]) - 1.0).abs() < 1e-6);

    // Mixed signs
    assert!((cosine_similarity(&[1.0, -1.0], &[1.0, -1.0]) - 1.0).abs() < 1e-6);
    assert!((cosine_similarity(&[1.0, -1.0], &[-1.0, 1.0]) - (-1.0)).abs() < 1e-6);
}

#[test]
fn test_cosine_similarity_properties() {
    // Symmetry: cos(a,b) = cos(b,a)
    let a = vec![1.0, 2.0, 3.0];
    let b = vec![4.0, 5.0, 6.0];
    assert_eq!(cosine_similarity(&a, &b), cosine_similarity(&b, &a));

    // Scale invariance: cos(a,b) = cos(ka,b) = cos(a,kb)
    let a = vec![1.0, 2.0, 3.0];
    let b = vec![4.0, 5.0, 6.0];
    let ka: Vec<f32> = a.iter().map(|x| x * 2.0).collect();
    assert!((cosine_similarity(&a, &b) - cosine_similarity(&ka, &b)).abs() < 1e-6);

    // Bounded: -1 <= cos(a,b) <= 1
    let test_pairs = vec![
        (vec![1.0, 0.0], vec![0.0, 1.0]),
        (vec![1.0, 2.0, 3.0], vec![-3.0, -2.0, -1.0]),
        (vec![5.0, 5.0, 5.0], vec![1.0, 1.0, 1.0]),
    ];

    for (a, b) in test_pairs {
        let sim = cosine_similarity(&a, &b);
        assert!(
            sim >= -1.0 && sim <= 1.0,
            "Similarity {} out of bounds",
            sim
        );
    }
}

#[test]
fn test_euclidean_distance_basic() {
    // Same point
    assert_eq!(euclidean_distance(&[0.0, 0.0], &[0.0, 0.0]), 0.0);

    // 3-4-5 triangle
    assert_eq!(euclidean_distance(&[0.0, 0.0], &[3.0, 4.0]), 5.0);

    // 1D distance
    assert_eq!(euclidean_distance(&[0.0], &[5.0]), 5.0);
    assert_eq!(euclidean_distance(&[5.0], &[0.0]), 5.0);

    // 3D distance
    let dist = euclidean_distance(&[1.0, 2.0, 3.0], &[4.0, 6.0, 8.0]);
    assert!((dist - 7.071).abs() < 0.01);
}

#[test]
fn test_euclidean_distance_edge_cases() {
    // Empty vectors
    assert_eq!(euclidean_distance(&[], &[]), 0.0);

    // Different lengths
    assert_eq!(euclidean_distance(&[1.0], &[1.0, 2.0]), f32::INFINITY);

    // Very large values - use smaller value to avoid overflow
    let large = 1e10;
    let dist = euclidean_distance(&[large], &[0.0]);
    assert!(dist.is_finite());
    assert!((dist - large).abs() < 1e-6);

    // Very small differences
    let a = 1.0;
    let b = 1.0 + f32::EPSILON;
    assert!(euclidean_distance(&[a], &[b]) > 0.0);
    assert!(euclidean_distance(&[a], &[b]) < 1e-6);
}

#[test]
fn test_euclidean_distance_properties() {
    // Non-negativity: d(a,b) >= 0
    let pairs = vec![
        (vec![0.0], vec![0.0]),
        (vec![1.0, 2.0], vec![3.0, 4.0]),
        (vec![-1.0, -2.0], vec![1.0, 2.0]),
    ];

    for (a, b) in pairs {
        assert!(euclidean_distance(&a, &b) >= 0.0);
    }

    // Symmetry: d(a,b) = d(b,a)
    let a = vec![1.0, 2.0, 3.0];
    let b = vec![4.0, 5.0, 6.0];
    assert_eq!(euclidean_distance(&a, &b), euclidean_distance(&b, &a));

    // Triangle inequality: d(a,c) <= d(a,b) + d(b,c)
    let a = vec![0.0, 0.0];
    let b = vec![1.0, 0.0];
    let c = vec![1.0, 1.0];
    let ab = euclidean_distance(&a, &b);
    let bc = euclidean_distance(&b, &c);
    let ac = euclidean_distance(&a, &c);
    assert!(ac <= ab + bc + 1e-6); // Small epsilon for floating point
}

#[test]
fn test_manhattan_distance_basic() {
    // Same point
    assert_eq!(manhattan_distance(&[0.0, 0.0], &[0.0, 0.0]), 0.0);

    // Simple cases
    assert_eq!(manhattan_distance(&[0.0, 0.0], &[1.0, 1.0]), 2.0);
    assert_eq!(manhattan_distance(&[0.0, 0.0], &[3.0, 4.0]), 7.0);

    // Negative coordinates
    assert_eq!(manhattan_distance(&[-1.0, -1.0], &[1.0, 1.0]), 4.0);
}

#[test]
fn test_manhattan_distance_edge_cases() {
    // Empty vectors
    assert_eq!(manhattan_distance(&[], &[]), 0.0);

    // Different lengths
    assert_eq!(manhattan_distance(&[1.0], &[1.0, 2.0]), f32::INFINITY);

    // Large values
    let large = 1e20;
    assert!(manhattan_distance(&[large], &[0.0]).is_finite());
}

#[test]
fn test_knn_search_basic() {
    let embeddings = vec![
        (0, vec![1.0, 0.0, 0.0]),
        (1, vec![0.0, 1.0, 0.0]),
        (2, vec![0.0, 0.0, 1.0]),
        (3, vec![0.5, 0.5, 0.0]),
        (4, vec![0.5, 0.0, 0.5]),
    ];

    // Search for nearest to [1, 0, 0]
    let query = vec![1.0, 0.0, 0.0];
    let results = knn_search(&query, &embeddings, 3, SimilarityMetric::Cosine);

    assert_eq!(results.len(), 3);
    assert_eq!(results[0].0, 0); // Exact match
    assert_eq!(results[0].1, 1.0);
}

#[test]
fn test_knn_search_metrics() {
    let embeddings = vec![
        (0, vec![0.0, 0.0]),
        (1, vec![1.0, 0.0]),
        (2, vec![0.0, 1.0]),
        (3, vec![1.0, 1.0]),
    ];

    let query = vec![0.5, 0.5];

    // Cosine similarity
    let cosine_results = knn_search(&query, &embeddings, 4, SimilarityMetric::Cosine);
    assert_eq!(cosine_results[0].0, 3); // [1,1] is most similar by angle

    // Euclidean distance (negated for consistency)
    let euclidean_results = knn_search(&query, &embeddings, 4, SimilarityMetric::Euclidean);
    // Just verify we got 4 results and they're valid
    assert_eq!(euclidean_results.len(), 4);
    // Check that all indices are valid
    for (idx, _) in &euclidean_results {
        assert!(*idx < 4);
    }

    // Manhattan distance
    let manhattan_results = knn_search(&query, &embeddings, 4, SimilarityMetric::Manhattan);
    // Just verify we got 4 results and they're valid
    assert_eq!(manhattan_results.len(), 4);
    for (idx, _) in &manhattan_results {
        assert!(*idx < 4);
    }
}

#[test]
fn test_knn_search_edge_cases() {
    // Empty embeddings
    let results = knn_search(&[1.0], &[], 5, SimilarityMetric::Cosine);
    assert_eq!(results.len(), 0);

    // k larger than dataset
    let embeddings = vec![(0, vec![1.0]), (1, vec![2.0])];
    let results = knn_search(&[1.5], &embeddings, 10, SimilarityMetric::Euclidean);
    assert_eq!(results.len(), 2);

    // k = 0
    let embeddings = vec![(0, vec![1.0])];
    let results = knn_search(&[1.0], &embeddings, 0, SimilarityMetric::Cosine);
    assert_eq!(results.len(), 0);
}

#[test]
fn test_kmeans_cluster_basic() {
    // Simple 2D clustering
    let embeddings = vec![
        vec![0.0, 0.0],
        vec![0.1, 0.1],
        vec![5.0, 5.0],
        vec![5.1, 5.1],
    ];

    let assignments = kmeans_cluster(&embeddings, 2, 100).unwrap();

    // Points 0,1 should be in one cluster, 2,3 in another
    assert_eq!(assignments.len(), 4);
    assert_eq!(assignments[0], assignments[1]);
    assert_eq!(assignments[2], assignments[3]);
    assert_ne!(assignments[0], assignments[2]);
}

#[test]
fn test_kmeans_cluster_edge_cases() {
    // Empty data
    let assignments = kmeans_cluster(&[], 3, 100).unwrap();
    assert_eq!(assignments.len(), 0);

    // k = 0
    let embeddings = vec![vec![1.0], vec![2.0]];
    let assignments = kmeans_cluster(&embeddings, 0, 100).unwrap();
    assert_eq!(assignments.len(), 0);

    // k > n
    let embeddings = vec![vec![1.0], vec![2.0]];
    let assignments = kmeans_cluster(&embeddings, 5, 100).unwrap();
    assert_eq!(assignments.len(), 2);

    // Single point
    let embeddings = vec![vec![1.0, 2.0, 3.0]];
    let assignments = kmeans_cluster(&embeddings, 1, 100).unwrap();
    assert_eq!(assignments, vec![0]);

    // All same points
    let embeddings = vec![vec![1.0]; 10];
    let assignments = kmeans_cluster(&embeddings, 3, 100).unwrap();
    assert_eq!(assignments.len(), 10);
}

#[test]
fn test_kmeans_convergence() {
    // Test that algorithm converges for well-separated clusters
    let mut embeddings = vec![];

    // Cluster 1 around (0, 0)
    for _ in 0..5 {
        embeddings.push(vec![
            0.0 + 0.1 * rand::random::<f32>(),
            0.0 + 0.1 * rand::random::<f32>(),
        ]);
    }

    // Cluster 2 around (10, 10)
    for _ in 0..5 {
        embeddings.push(vec![
            10.0 + 0.1 * rand::random::<f32>(),
            10.0 + 0.1 * rand::random::<f32>(),
        ]);
    }

    let assignments = kmeans_cluster(&embeddings, 2, 100).unwrap();

    // Check that points are correctly clustered
    let cluster1 = assignments[0];
    let cluster2 = assignments[5];
    assert_ne!(cluster1, cluster2);

    // All points in first group should be in same cluster
    for i in 0..5 {
        assert_eq!(assignments[i], cluster1);
    }

    // All points in second group should be in same cluster
    for i in 5..10 {
        assert_eq!(assignments[i], cluster2);
    }
}

// Property-based tests
proptest! {
    #[test]
    fn prop_cosine_similarity_bounded(
        a in prop::collection::vec(-100.0f32..100.0, 1..10),
        b in prop::collection::vec(-100.0f32..100.0, 1..10)
    ) {
        if a.len() == b.len() {
            let sim = cosine_similarity(&a, &b);
            prop_assert!(sim >= -1.001 && sim <= 1.001); // Small epsilon for float errors
        }
    }

    #[test]
    fn prop_cosine_similarity_symmetric(
        a in prop::collection::vec(-100.0f32..100.0, 1..10),
        b in prop::collection::vec(-100.0f32..100.0, 1..10)
    ) {
        if a.len() == b.len() {
            let sim_ab = cosine_similarity(&a, &b);
            let sim_ba = cosine_similarity(&b, &a);
            prop_assert!((sim_ab - sim_ba).abs() < 1e-6);
        }
    }

    #[test]
    fn prop_euclidean_distance_non_negative(
        a in prop::collection::vec(-100.0f32..100.0, 1..10),
        b in prop::collection::vec(-100.0f32..100.0, 1..10)
    ) {
        if a.len() == b.len() {
            let dist = euclidean_distance(&a, &b);
            prop_assert!(dist >= 0.0);
        }
    }

    #[test]
    fn prop_manhattan_distance_triangle_inequality(
        a in prop::collection::vec(-10.0f32..10.0, 3),
        b in prop::collection::vec(-10.0f32..10.0, 3),
        c in prop::collection::vec(-10.0f32..10.0, 3)
    ) {
        let ab = manhattan_distance(&a, &b);
        let bc = manhattan_distance(&b, &c);
        let ac = manhattan_distance(&a, &c);
        prop_assert!(ac <= ab + bc + 1e-4); // Use larger epsilon for floating point precision
    }

    #[test]
    fn prop_knn_returns_k_or_less(
        query in prop::collection::vec(0.0f32..1.0, 5),
        k in 0usize..20
    ) {
        let embeddings = vec![
            (0, vec![0.1, 0.2, 0.3, 0.4, 0.5]),
            (1, vec![0.5, 0.4, 0.3, 0.2, 0.1]),
            (2, vec![0.3, 0.3, 0.3, 0.3, 0.3]),
        ];

        let results = knn_search(&query, &embeddings, k, SimilarityMetric::Cosine);
        prop_assert!(results.len() <= k.min(embeddings.len()));
    }

    #[test]
    fn prop_kmeans_assignments_valid(
        k in 1usize..5,
        n in 1usize..20
    ) {
        let embeddings: Vec<Vec<f32>> = (0..n)
            .map(|i| vec![i as f32, (i * 2) as f32])
            .collect();

        let assignments = kmeans_cluster(&embeddings, k, 10).unwrap();

        prop_assert_eq!(assignments.len(), n);
        for &assignment in &assignments {
            prop_assert!(assignment < k);
        }
    }
}
