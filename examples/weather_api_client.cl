; Weather API Client Example
; This demonstrates building a weather application using network effects

; Configuration
(let ((api-key "your-api-key-here")
      (base-url "https://api.openweathermap.org/data/2.5"))

  ; Function to build API URL with parameters
  (let ((build-url (lambda (endpoint params)
                     (let ((query-string (effect network:url-encode 
                                               (merge params {"appid" api-key}))))
                       (string-concat base-url endpoint "?" query-string)))))

    ; Function to fetch weather for a city
    (let ((get-weather (lambda (city)
                         (let ((url (build-url "/weather" {"q" city "units" "metric"}))
                               (response (effect network:http-get url)))
                           (if (= (get response "status") 200)
                               (json-parse (get response "body"))
                               (error "Failed to fetch weather")))))

      ; Function to fetch forecast
      (let ((get-forecast (lambda (city days)
                           (let ((url (build-url "/forecast" 
                                               {"q" city 
                                                "cnt" (to-string (* days 8))  ; 8 entries per day
                                                "units" "metric"}))
                                 (response (effect network:http-get url)))
                             (if (= (get response "status") 200)
                                 (json-parse (get response "body"))
                                 (error "Failed to fetch forecast")))))

        ; Function to display weather info
        (let ((display-weather (lambda (weather-data)
                                (let ((main (get weather-data "main"))
                                      (weather (car (get weather-data "weather")))
                                      (city (get weather-data "name")))
                                  (do
                                    (print "Weather in" city)
                                    (print "Temperature:" (get main "temp") "°C")
                                    (print "Feels like:" (get main "feels_like") "°C")
                                    (print "Condition:" (get weather "description"))
                                    (print "Humidity:" (get main "humidity") "%")
                                    (print "Wind speed:" (get (get weather-data "wind") "speed") "m/s")))))

          ; Function to display forecast
          (let ((display-forecast (lambda (forecast-data)
                                   (let ((city (get (get forecast-data "city") "name"))
                                         (entries (get forecast-data "list")))
                                     (do
                                       (print "\nForecast for" city)
                                       (map (lambda (entry)
                                              (let ((dt (get entry "dt_txt"))
                                                    (temp (get (get entry "main") "temp"))
                                                    (desc (get (car (get entry "weather")) "description")))
                                                (print dt "-" temp "°C -" desc)))
                                            (take 8 entries))))))  ; Show first day only

            ; Error handler for network issues
            (let ((with-error-handling (lambda (action)
                                         (handler
                                           ((error (lambda (err)
                                                    (print "Error:" (get err "message"))
                                                    nil)))
                                           (action)))))

              ; Main weather application
              (let ((weather-app (lambda (cities)
                                  (do
                                    (print "=== Weather Application ===\n")
                                    
                                    ; Fetch current weather for multiple cities in parallel
                                    (print "Current Weather:")
                                    (let ((weather-results (parallel-map 
                                                           (lambda (city)
                                                             (with-error-handling
                                                               (lambda ()
                                                                 (cons city (get-weather city)))))
                                                           cities)))
                                      (map (lambda (result)
                                             (if (cdr result)
                                                 (display-weather (cdr result))
                                                 (print "Failed to get weather for" (car result))))
                                           weather-results))
                                    
                                    ; Get forecast for first city
                                    (print "\n" (string-repeat "-" 40) "\n")
                                    (let ((forecast (with-error-handling
                                                     (lambda ()
                                                       (get-forecast (car cities) 1)))))
                                      (if forecast
                                          (display-forecast forecast)
                                          (print "Failed to get forecast"))))))

                ; Example usage with caching
                (let ((cached-weather-app (lambda (cities)
                                           (let ((cache {}))
                                             (do
                                               ; Check cache first
                                               (map (lambda (city)
                                                      (if (get cache city)
                                                          (print "Using cached data for" city)
                                                          (do
                                                            (print "Fetching fresh data for" city)
                                                            (let ((data (get-weather city)))
                                                              (set! cache city data)))))
                                                    cities)
                                               
                                               ; Run the app with cached data
                                               (weather-app cities))))))

                  ; Run the application
                  (cached-weather-app ["London" "New York" "Tokyo"]))))))))))

; Alternative: Using WebSocket for real-time weather updates
(let ((weather-stream (lambda (city)
                       (let ((ws-url "wss://weather-stream.example.com/live"))
                         (handler
                           ((error (lambda (err)
                                    (print "WebSocket error:" (get err "message"))
                                    nil)))
                           
                           (let ((ws (effect network:ws-connect ws-url)))
                             (do
                               ; Subscribe to city updates
                               (effect network:ws-send ws 
                                      (json-stringify {"action" "subscribe" 
                                                      "city" city}))
                               
                               ; Listen for updates
                               (let ((listen-loop (lambda (count)
                                                   (if (> count 0)
                                                       (let ((msg (effect network:ws-receive ws 5.0)))
                                                         (if msg
                                                             (do
                                                               (let ((data (json-parse (get msg "data"))))
                                                                 (print "Update:" (get data "temp") "°C"))
                                                               (listen-loop (- count 1)))
                                                             (print "No update received")))
                                                       nil))))
                                 (listen-loop 10))  ; Listen for 10 updates
                               
                               ; Clean up
                               (effect network:ws-close ws))))))))
  
  ; Example with retry logic
  (let ((with-retry (lambda (action max-retries)
                     (let ((attempt (lambda (n)
                                     (handler
                                       ((error (lambda (err)
                                                (if (< n max-retries)
                                                    (do
                                                      (print "Retry" (+ n 1) "after error:" 
                                                             (get err "message"))
                                                      (effect time:sleep (* n 1.0))  ; Exponential backoff
                                                      (attempt (+ n 1)))
                                                    (error "Max retries exceeded")))))
                                       (action)))))
                       (attempt 0)))))
    
    ; Fetch with automatic retry
    (with-retry 
      (lambda () (get-weather "London"))
      3)))