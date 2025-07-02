; Concurrency Demo
; This demonstrates ClaudeLang's concurrency primitives

; 1. Basic channel operations
(print "=== Basic Channels ===")
(let ((ch (chan)))
  (do
    ; Send and receive
    (send! ch "Hello, Channel!")
    (let ((result (receive! ch)))
      (print "Received:" (get result "value")))))

; 2. Buffered channels
(print "\n=== Buffered Channels ===")
(let ((ch (chan 3)))
  (do
    ; Can send multiple values without blocking
    (send! ch "First")
    (send! ch "Second")
    (send! ch "Third")
    
    ; Receive them
    (print "1:" (get (receive! ch) "value"))
    (print "2:" (get (receive! ch) "value"))
    (print "3:" (get (receive! ch) "value"))))

; 3. Goroutines
(print "\n=== Goroutines ===")
(let ((ch (chan)))
  (do
    ; Launch goroutine
    (go (do
          (print "Goroutine: Starting work...")
          (effect time:sleep 0.1)
          (send! ch "Work complete!")
          (print "Goroutine: Done")))
    
    ; Main continues
    (print "Main: Waiting for goroutine...")
    (print "Main: Got result:" (get (receive! ch) "value"))))

; 4. Multiple goroutines
(print "\n=== Multiple Goroutines ===")
(let ((results (chan 5)))
  (do
    ; Launch 5 workers
    (map (lambda (id)
          (go (do
                (effect time:sleep (* id 0.01))
                (send! results (string-concat "Worker " (to-string id) " done")))))
        [1 2 3 4 5])
    
    ; Collect results
    (map (lambda (_)
          (print (get (receive! results) "value")))
        [1 2 3 4 5])))

; 5. Producer-Consumer pattern
(print "\n=== Producer-Consumer ===")
(let ((jobs (chan 10))
      (results (chan 10)))
  (do
    ; Producer
    (go (do
          (map (lambda (i)
                (do
                  (print "Producing job" i)
                  (send! jobs i)))
              [1 2 3 4 5])
          (effect concurrent:close jobs)))
    
    ; Consumers (3 workers)
    (map (lambda (worker-id)
          (go (let ((process-jobs (lambda ()
                                   (let ((job (receive! jobs)))
                                     (if (get job "ok")
                                         (do
                                           (print (string-concat "Worker " (to-string worker-id) 
                                                               " processing job " 
                                                               (to-string (get job "value"))))
                                           (effect time:sleep 0.02)
                                           (send! results (* (get job "value") (get job "value")))
                                           (process-jobs))
                                         (print (string-concat "Worker " (to-string worker-id) 
                                                             " done")))))))
                (process-jobs))))
        [1 2 3])
    
    ; Collect results
    (effect time:sleep 0.2)
    (let ((collected []))
      (let ((collect (lambda (n)
                      (if (> n 0)
                          (let ((r (receive! results)))
                            (if (get r "ok")
                                (do
                                  (set! collected (cons (get r "value") collected))
                                  (collect (- n 1)))
                                collected))
                          collected))))
        (print "Results:" (sort (collect 5)))))))

; 6. Mutex for shared state
(print "\n=== Mutex Example ===")
(let ((mu (effect concurrent:mutex))
      (counter 0)
      (ch (chan)))
  (do
    ; Launch 10 goroutines that increment counter
    (map (lambda (id)
          (go (do
                (map (lambda (_)
                      (do
                        (effect concurrent:lock mu)
                        (let ((old counter))
                          (set! counter (+ counter 1))
                          (print (string-concat "Goroutine " (to-string id) 
                                              ": " (to-string old) 
                                              " -> " (to-string counter))))
                        (effect concurrent:unlock mu)))
                    [1 2 3])
                (send! ch "done"))))
        [1 2 3])
    
    ; Wait for all to complete
    (map (lambda (_) (receive! ch)) [1 2 3])
    (print "Final counter:" counter)))

; 7. Semaphore for limited concurrency
(print "\n=== Semaphore Example ===")
(let ((sem (effect concurrent:semaphore 2))  ; Allow max 2 concurrent
      (ch (chan)))
  (do
    (map (lambda (id)
          (go (do
                (print (string-concat "Worker " (to-string id) " waiting..."))
                (effect concurrent:acquire sem)
                (print (string-concat "Worker " (to-string id) " working..."))
                (effect time:sleep 0.05)
                (print (string-concat "Worker " (to-string id) " done"))
                (effect concurrent:release sem)
                (send! ch id))))
        [1 2 3 4 5])
    
    ; Wait for all
    (map (lambda (_) (receive! ch)) [1 2 3 4 5])
    (print "All workers complete")))

; 8. Pipeline pattern
(print "\n=== Pipeline Pattern ===")
(let ((stage1 (chan))
      (stage2 (chan))
      (stage3 (chan)))
  (do
    ; Stage 1: Generate numbers
    (go (do
          (map (lambda (i) (send! stage1 i)) [1 2 3 4 5])
          (effect concurrent:close stage1)))
    
    ; Stage 2: Square numbers
    (go (let ((process (lambda ()
                        (let ((r (receive! stage1)))
                          (if (get r "ok")
                              (do
                                (let ((val (get r "value")))
                                  (send! stage2 (* val val)))
                                (process))
                              (effect concurrent:close stage2))))))
          (process)))
    
    ; Stage 3: Add 10
    (go (let ((process (lambda ()
                        (let ((r (receive! stage2)))
                          (if (get r "ok")
                              (do
                                (let ((val (get r "value")))
                                  (send! stage3 (+ val 10)))
                                (process))
                              (effect concurrent:close stage3))))))
          (process)))
    
    ; Collect results
    (let ((results []))
      (let ((collect (lambda ()
                      (let ((r (receive! stage3)))
                        (if (get r "ok")
                            (do
                              (set! results (cons (get r "value") results))
                              (collect))
                            (reverse results))))))
        (print "Pipeline results:" (collect))))))

; 9. Fan-out/Fan-in
(print "\n=== Fan-out/Fan-in ===")
(let ((work-ch (chan))
      (result-ch (chan)))
  (do
    ; Create workers
    (let ((worker (lambda (id)
                   (let ((loop (lambda ()
                                (let ((job (receive! work-ch)))
                                  (if (get job "ok")
                                      (do
                                        (let ((n (get job "value")))
                                          (print (string-concat "Worker " (to-string id) 
                                                              " processing " (to-string n)))
                                          (send! result-ch (* n n)))
                                        (loop))
                                      (print (string-concat "Worker " (to-string id) 
                                                          " shutting down")))))))
                     (loop)))))
      
      ; Start 3 workers
      (map (lambda (id) (go (worker id))) [1 2 3]))
    
    ; Send work
    (map (lambda (i) (send! work-ch i)) [10 20 30 40 50])
    
    ; Collect results
    (let ((results []))
      (do
        (map (lambda (_)
              (let ((r (receive! result-ch)))
                (set! results (cons (get r "value") results))))
            [1 2 3 4 5])
        (print "Results:" (sort results))))
    
    ; Shutdown workers
    (effect concurrent:close work-ch)))

; 10. Concurrent map implementation
(print "\n=== Concurrent Map ===")
(let ((concurrent-map (lambda (f items)
                       (let ((n (length items))
                             (ch (chan n)))
                         (do
                           ; Launch goroutine for each item
                           (map (lambda (item)
                                 (go (send! ch (f item))))
                               items)
                           ; Collect results
                           (map (lambda (_)
                                 (get (receive! ch) "value"))
                               items))))))
  
  ; Use it to compute squares concurrently
  (let ((numbers [1 2 3 4 5 6 7 8 9 10]))
    (print "Squares:" (concurrent-map (lambda (x) (* x x)) numbers))))

(print "\n=== Concurrency demo complete! ===")