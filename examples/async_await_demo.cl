; Async/Await Demo
; This demonstrates ClaudeLang's async/await primitives

; 1. Simple promise creation and resolution
(let ((simple-promise (promise (lambda (resolve reject)
                                (resolve "Hello from promise!")))))
  (print "Simple promise result:" (await simple-promise)))

; 2. Async functions
(let ((fetch-user (async (user-id)
                    (promise (lambda (resolve reject)
                              ; Simulate API call
                              (effect time:sleep 0.1)
                              (resolve {"id" user-id
                                       "name" (string-concat "User " (to-string user-id))
                                       "email" (string-concat "user" (to-string user-id) "@example.com")})))))
      
      (fetch-posts (async (user-id)
                     (promise (lambda (resolve reject)
                               ; Simulate API call
                               (effect time:sleep 0.05)
                               (resolve [{"id" 1 "userId" user-id "title" "First Post"}
                                        {"id" 2 "userId" user-id "title" "Second Post"}]))))))
  
  ; Fetch user and their posts
  (let ((user (await (fetch-user 123))))
    (print "User:" user)
    (let ((posts (await (fetch-posts (get user "id")))))
      (print "Posts:" posts))))

; 3. Parallel async operations
(print "\nParallel operations:")
(let ((task1 (async () 
                (do
                  (print "Task 1 starting...")
                  (effect time:sleep 0.1)
                  (print "Task 1 done!")
                  "Result 1")))
      (task2 (async ()
                (do
                  (print "Task 2 starting...")
                  (effect time:sleep 0.05)
                  (print "Task 2 done!")
                  "Result 2")))
      (task3 (async ()
                (do
                  (print "Task 3 starting...")
                  (effect time:sleep 0.08)
                  (print "Task 3 done!")
                  "Result 3"))))
  
  ; Run all tasks in parallel
  (let ((results (await (effect async:all [(task1) (task2) (task3)]))))
    (print "All results:" results)))

; 4. Promise chaining
(print "\nPromise chaining:")
(let ((initial (promise (lambda (resolve reject) (resolve 10)))))
  (let ((doubled (effect async:then initial (lambda (x) (* x 2))))
        (squared (effect async:then doubled (lambda (x) (* x x)))))
    (print "Initial: 10")
    (print "Doubled:" (await doubled))
    (print "Squared:" (await squared))))

; 5. Error handling
(print "\nError handling:")
(let ((risky-operation (async ()
                         (promise (lambda (resolve reject)
                                   (if (< (effect random:random) 0.5)
                                       (resolve "Success!")
                                       (reject "Random failure!")))))))
  (handler
    ((error (lambda (err)
              (print "Caught error:" (get err "message"))
              "Recovered from error")))
    (print "Result:" (await (risky-operation)))))

; 6. Race conditions
(print "\nRace example:")
(let ((slow-task (effect async:delay 100 "I'm slow"))
      (fast-task (effect async:delay 20 "I'm fast!"))
      (medium-task (effect async:delay 50 "I'm medium")))
  (print "Winner:" (await (effect async:race [slow-task fast-task medium-task]))))

; 7. Async with network effects
(print "\nAsync network example:")
(let ((fetch-weather (async (city)
                       (promise (lambda (resolve reject)
                                 ; Simulate network request
                                 (handler
                                   ((error (lambda (err) (reject err))))
                                   (let ((response (effect network:http-get 
                                                          (string-concat "https://api.weather.com/" city))))
                                     (resolve {"city" city
                                              "temp" (+ 15 (* 10 (effect random:random)))
                                              "condition" (if (< (effect random:random) 0.5)
                                                             "Sunny"
                                                             "Cloudy")}))))))))
  
  ; Fetch weather for multiple cities in parallel
  (let ((cities ["London" "Paris" "Tokyo" "New York"]))
    (let ((weather-promises (map fetch-weather cities)))
      (let ((all-weather (await (effect async:all weather-promises))))
        (map (lambda (w)
               (print (string-concat (get w "city") ": "
                                   (to-string (get w "temp")) "°C, "
                                   (get w "condition"))))
             all-weather)))))

; 8. Async reduce
(print "\nAsync reduce example:")
(let ((async-sum (lambda (numbers)
                   (let ((add-async (async (acc n)
                                      (promise (lambda (resolve reject)
                                                (effect time:sleep 0.01)
                                                (resolve (+ acc n)))))))
                     (reduce (lambda (acc-promise n)
                              (let ((acc (await acc-promise)))
                                (add-async acc n)))
                            (promise (lambda (resolve reject) (resolve 0)))
                            numbers)))))
  (print "Sum of [1..10]:" (await (async-sum [1 2 3 4 5 6 7 8 9 10]))))

; 9. Timeout handling
(print "\nTimeout example:")
(let ((slow-operation (async ()
                        (promise (lambda (resolve reject)
                                  (effect time:sleep 5.0)
                                  (resolve "This takes too long!")))))
      (with-timeout (lambda (operation timeout-ms)
                     (let ((timeout-promise (effect async:delay timeout-ms "Timeout!")))
                       (effect async:race [operation timeout-promise])))))
  (print "Result:" (await (with-timeout (slow-operation) 100))))

; 10. Async recursion
(print "\nAsync recursion:")
(let ((fetch-page (async (page)
                    (promise (lambda (resolve reject)
                              (effect time:sleep 0.05)
                              (resolve {"page" page
                                       "data" (string-concat "Data from page " (to-string page))
                                       "hasNext" (< page 3)})))))
      
      (fetch-all-pages (lambda (page acc)
                        (let ((result (await (fetch-page page))))
                          (let ((new-acc (cons (get result "data") acc)))
                            (if (get result "hasNext")
                                (fetch-all-pages (+ page 1) new-acc)
                                (reverse new-acc)))))))
  
  (print "All pages:" (fetch-all-pages 1 [])))

(print "\nAsync/await demo complete!")