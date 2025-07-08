; Test simpler handler without concat
(handler
  ((error (lambda (err) "Caught error!")))
  (effect error:raise "test error"))