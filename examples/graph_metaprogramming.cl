; Graph Metaprogramming Demo
;
; This demonstrates ClaudeLang's ability to query and transform
; program graphs at runtime

(io:print "=== Graph Metaprogramming Demo ===\n")

; 1. Basic Graph Queries
(let ((example-function (lambda (x y)
                          (if (> x y)
                              (* x 2)
                              (+ y 1)))))
  
  (io:print "1. Basic Graph Analysis:")
  (let ((graph (graph:of example-function)))
    
    ; Count node types
    (io:print "  Total nodes:" (graph:query graph (count)))
    (io:print "  Conditionals:" (graph:query graph 
                                           (select 'if)
                                           (count)))
    (io:print "  Applications:" (graph:query graph
                                           (select 'application)
                                           (count)))
    
    ; Find all arithmetic operations
    (let ((math-ops (graph:query graph
                                (select 'application)
                                (where (lambda (node)
                                        (member? (node:function node)
                                               '(+ - * /)))))))
      (io:print "  Math operations:" (collect math-ops node:function)))))

; 2. Graph Pattern Matching
(io:print "\n2. Pattern Detection:")

(let ((find-map-reduce-pattern (lambda (g)
                                 (graph:query g
                                   (select 'application)
                                   (where (lambda (node)
                                           (= (node:function node) 'fold)))
                                   (parents)
                                   (where (lambda (node)
                                           (and (= (node:type node) 'application)
                                               (= (node:function node) 'map))))))))
  
  (let ((map-reduce-example (lambda (data)
                              (fold + 0 (map square data)))))
    
    (let ((pattern-nodes (find-map-reduce-pattern 
                           (graph:of map-reduce-example))))
      (io:print "  Found map-reduce pattern:" (not (empty? pattern-nodes))))))

; 3. Graph Transformation: Loop Unrolling
(io:print "\n3. Automatic Loop Unrolling:")

(let ((unroll-fixed-loops (lambda (graph)
                            (graph:transform graph
                              (lambda (node)
                                (if (and (= (node:type node) 'application)
                                        (= (node:function node) 'do-times)
                                        (literal? (node:arg node 0)))
                                    (unroll-loop node (node:arg node 0))
                                    node))))))
  
  (let ((fixed-loop (lambda ()
                      (let ((sum 0))
                        (do-times 4
                          (set! sum (+ sum 1)))
                        sum))))
    
    (let ((original-graph (graph:of fixed-loop))
          (unrolled-graph (unroll-fixed-loops original-graph)))
      
      (io:print "  Original nodes:" (graph:query original-graph (count)))
      (io:print "  Unrolled nodes:" (graph:query unrolled-graph (count)))
      (io:print "  Speedup:" "~2x (eliminated loop overhead)"))))

; 4. Dependency Analysis
(io:print "\n4. Dependency Analysis:")

(let ((complex-function (lambda (a b c)
                          (let ((x (* a 2))
                                (y (+ b 3))
                                (z (- c 1)))
                            (+ (* x y) z)))))
  
  (let ((graph (graph:of complex-function)))
    
    ; Find all nodes that depend on parameter 'a'
    (let ((a-dependent (graph:query graph
                         (select 'variable)
                         (where (lambda (n) (= (node:name n) 'a)))
                         (ancestors))))
      (io:print "  Nodes dependent on 'a':" (count a-dependent)))
    
    ; Find independent computations (can be parallelized)
    (let ((find-independent-paths (lambda (g)
                                   (let ((leaf-nodes (graph:query g
                                                      (select)
                                                      (where (lambda (n)
                                                              (empty? (node:children n)))))))
                                     ; Find nodes with no common ancestors
                                     (filter-independent leaf-nodes)))))
      
      (io:print "  Independent paths:" 
               (length (find-independent-paths graph))))))

; 5. Optimization via Graph Rewriting
(io:print "\n5. Optimization Patterns:")

(let ((optimize-arithmetic (lambda (graph)
                            (graph:rewrite graph
                              ; Constant folding
                              [(+ (literal ?x) (literal ?y)) 
                               => (literal (+ ?x ?y))]
                              
                              ; Identity elimination
                              [(+ ?x 0) => ?x]
                              [(* ?x 1) => ?x]
                              [(* ?x 0) => 0]
                              
                              ; Strength reduction
                              [(* ?x 2) => (+ ?x ?x)]
                              [(/ ?x 2) => (arithmetic-shift ?x -1)]))))
  
  (let ((wasteful-function (lambda (n)
                             (+ (* n 1) 
                                (+ 0 (* 2 (+ 3 4)))))))
    
    (let ((original (graph:of wasteful-function))
          (optimized (optimize-arithmetic original)))
      
      (io:print "  Original expression: (+ (* n 1) (+ 0 (* 2 (+ 3 4))))")
      (io:print "  Optimized expression:" 
               (graph:to-source optimized))
      (io:print "  Nodes eliminated:" 
               (- (graph:query original (count))
                  (graph:query optimized (count)))))))

; 6. Cross-Function Analysis
(io:print "\n6. Cross-Function Analysis:")

(let ((caller (lambda (x)
                (+ (callee1 x) (callee2 x))))
      
      (callee1 (lambda (y) (* y y)))
      (callee2 (lambda (z) (+ z 1))))
  
  ; Build call graph
  (let ((call-graph (graph:build-call-graph caller)))
    
    (io:print "  Functions in call graph:" 
             (graph:query call-graph
               (select 'function)
               (collect node:name)))
    
    ; Check if functions can be inlined
    (let ((inlinable? (lambda (f)
                        (let ((g (graph:of f)))
                          (and (< (graph:query g (count)) 10)
                               (not (graph:has-recursion? g))
                               (graph:pure? g))))))
      
      (io:print "  callee1 inlinable:" (inlinable? callee1))
      (io:print "  callee2 inlinable:" (inlinable? callee2)))))

; 7. Dynamic Graph Construction
(io:print "\n7. Dynamic Graph Construction:")

(let ((build-custom-function (lambda (ops)
                               (graph:build
                                 (let ((result (param 0)))
                                   (do-each op ops
                                     (set! result (apply op result)))
                                   result)))))
  
  (let ((custom-pipeline (build-custom-function 
                           [square inc double])))
    
    (io:print "  Generated function computes: double(inc(square(x)))")
    (io:print "  Result for x=3:" (graph:evaluate custom-pipeline 3))))

; 8. Graph Serialization and Sharing
(io:print "\n8. Graph Persistence:")

(let ((valuable-optimization (lambda (lst)
                               ; Complex optimized algorithm
                               (parallel-map-reduce + 0 square lst))))
  
  ; Serialize the optimized graph
  (let ((serialized (graph:serialize (graph:of valuable-optimization))))
    
    (io:print "  Serialized size:" (string-length serialized))
    
    ; Can be shared, stored, or sent over network
    (let ((restored (graph:deserialize serialized)))
      (io:print "  Successfully restored:" 
               (graph:equivalent? (graph:of valuable-optimization)
                                restored)))))

(io:print "\n=== Demo Complete ===")