; Test effect with handler
(handler
  ((error (lambda (err) (concat "Caught: " err))))
  (effect error:raise "test error"))