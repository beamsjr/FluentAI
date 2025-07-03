; FluentAi Database Example
; Demonstrates the functional database effect system

; Define a user schema
(define user-schema
  {:id :int
   :name :string
   :email :string
   :age :int
   :created_at :timestamp})

; Define a posts schema
(define post-schema
  {:id :int
   :user_id :int
   :title :string
   :content :text
   :published :bool
   :created_at :timestamp})

; Initialize database connection
(effect db:connect "postgresql://localhost/claudelang_example")

; Create tables (would normally be done via migrations)
(effect db:execute "
  CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    age INTEGER CHECK (age >= 0),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")

(effect db:execute "
  CREATE TABLE IF NOT EXISTS posts (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    title VARCHAR(200) NOT NULL,
    content TEXT NOT NULL,
    published BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")

; Insert a user
(define (create-user name email age)
  (effect db:execute 
    "INSERT INTO users (name, email, age) VALUES (?, ?, ?)"
    [name email age]))

; Query users
(define (find-users-by-age min-age)
  (effect db:query
    "SELECT * FROM users WHERE age >= ?"
    [min-age]))

; Functional query builder example
(define (find-active-users)
  (db:from 'users
    (db:where (db:and 
      (db:gt 'age 18)
      (db:not-null 'email)))
    (db:select '(id name email))
    (db:order-by 'name :asc)
    (db:limit 100)))

; Transaction example
(define (create-user-with-post name email age title content)
  (effect db:begin-transaction)
  (let ((user-result (create-user name email age)))
    (if (error? user-result)
      (begin
        (effect db:rollback-transaction)
        (error:raise "user-creation" "Failed to create user"))
      (let ((user-id (get-user-id user-result)))
        (let ((post-result (effect db:execute
                            "INSERT INTO posts (user_id, title, content) VALUES (?, ?, ?)"
                            [user-id title content])))
          (if (error? post-result)
            (begin
              (effect db:rollback-transaction)
              (error:raise "post-creation" "Failed to create post"))
            (begin
              (effect db:commit-transaction)
              {:user-id user-id :post-id (get-post-id post-result)})))))))

; Aggregate query example
(define (user-statistics)
  (let ((stats (effect db:query "
    SELECT 
      COUNT(*) as total_users,
      AVG(age) as avg_age,
      MIN(age) as min_age,
      MAX(age) as max_age
    FROM users")))
    (io:print "User Statistics:" stats)))

; Join query example
(define (users-with-posts)
  (effect db:query "
    SELECT 
      u.name,
      u.email,
      COUNT(p.id) as post_count,
      MAX(p.created_at) as last_post_date
    FROM users u
    LEFT JOIN posts p ON u.id = p.user_id
    GROUP BY u.id, u.name, u.email
    HAVING COUNT(p.id) > 0
    ORDER BY post_count DESC"))

; Prepared statement example
(define find-user-by-email-stmt
  (effect db:prepare "find-user-by-email" 
    "SELECT * FROM users WHERE email = ?"))

(define (find-user-by-email email)
  (effect db:execute-prepared "find-user-by-email" [email]))

; Main example usage
(define (main)
  ; Create some users
  (create-user "Alice" "alice@example.com" 25)
  (create-user "Bob" "bob@example.com" 30)
  (create-user "Charlie" "charlie@example.com" 35)
  
  ; Create a user with a post in a transaction
  (create-user-with-post 
    "Diana" 
    "diana@example.com" 
    28
    "My First Post"
    "This is the content of my first post!")
  
  ; Query users
  (io:print "Users over 25:")
  (let ((users (find-users-by-age 25)))
    (for-each (lambda (user) (io:print user)) users))
  
  ; Show statistics
  (user-statistics)
  
  ; Show users with posts
  (io:print "\nUsers with posts:")
  (let ((users (users-with-posts)))
    (for-each (lambda (user) (io:print user)) users))
  
  ; Use prepared statement
  (io:print "\nFinding user by email:")
  (let ((user (find-user-by-email "alice@example.com")))
    (io:print user))
  
  ; Check connection status
  (io:print "\nDatabase connected:" (effect db:is-connected))
  (io:print "Database stats:" (effect db:stats)))

; Error handling example
(define (safe-database-operation)
  (error:catch "db-error"
    (lambda (err)
      (io:print "Database error occurred:" err)
      (effect db:rollback-transaction)
      nil)))

; Run the example
(main)