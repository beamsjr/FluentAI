(letrec ((f (lambda (x) (f (- x 1)))))
  (f 5))
EOF < /dev/null