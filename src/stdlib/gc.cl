; Garbage Collection module for ClaudeLang
; Provides functions to control and monitor garbage collection

(module gc
  (export collect stats enable disable set-threshold with-gc-disabled))

; Force a garbage collection cycle
(define (collect)
  "Force a full garbage collection cycle"
  (effect:io
    (primitive:gc-collect)))

; Get garbage collection statistics
(define (stats)
  "Get current GC statistics as a dictionary"
  (effect:io
    (primitive:gc-stats)))

; Enable garbage collection
(define (enable)
  "Enable automatic garbage collection"
  (effect:io
    (primitive:gc-enable #t)))

; Disable garbage collection
(define (disable)
  "Disable automatic garbage collection (use with caution)"
  (effect:io
    (primitive:gc-enable #f)))

; Set collection threshold
(define (set-threshold n)
  "Set the number of allocations before automatic collection"
  (effect:io
    (primitive:gc-set-threshold n)))

; Run code with GC temporarily disabled
(define-syntax with-gc-disabled
  (syntax-rules ()
    ((with-gc-disabled body ...)
     (begin
       (gc:disable)
       (let ((result (begin body ...)))
         (gc:enable)
         (gc:collect)  ; Collect after re-enabling
         result)))))

; Memory pressure indication
(define (memory-pressure)
  "Get current memory pressure (0.0 to 1.0)"
  (let ((s (stats)))
    (/ (dict:get s "live_objects" 0)
       (max 1 (dict:get s "total_allocations" 1)))))

; Allocation rate monitoring
(define (allocation-rate)
  "Get current allocation rate (objects per collection)"
  (let ((s (stats)))
    (/ (dict:get s "total_allocations" 0)
       (max 1 (dict:get s "collections" 1)))))

; Check if object will be collected
(define (will-collect? obj)
  "Check if an object has no references and will be collected"
  (effect:io
    (primitive:gc-will-collect obj)))

; Get object reference count (for debugging)
(define (ref-count obj)
  "Get reference count for a GC-managed object"
  (effect:io
    (primitive:gc-ref-count obj)))

; Memory usage estimation
(define (estimate-memory)
  "Estimate current memory usage in objects"
  (dict:get (stats) "live_objects" 0))

; Run function and report GC impact
(define (profile-gc thunk)
  "Run a function and report GC statistics"
  (let ((before (stats)))
    (let ((result (thunk)))
      (let ((after (stats)))
        (print "GC Profile:")
        (print "  Objects allocated:" 
               (- (dict:get after "total_allocations" 0)
                  (dict:get before "total_allocations" 0)))
        (print "  Objects collected:"
               (- (dict:get after "objects_collected" 0)
                  (dict:get before "objects_collected" 0)))
        (print "  Collections run:"
               (- (dict:get after "collections" 0)
                  (dict:get before "collections" 0)))
        (print "  Live objects:" (dict:get after "live_objects" 0))
        result))))