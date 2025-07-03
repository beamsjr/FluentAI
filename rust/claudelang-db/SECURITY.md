# Security Guide for ClaudeLang Database Module

## SQL Injection Prevention

The ClaudeLang database module is designed with security as a top priority. All database operations use **parameterized queries** to prevent SQL injection attacks.

### ✅ Safe Practices (Always Use These)

#### 1. Use Parameterized Queries

**ClaudeLang (Safe):**
```lisp
;; Parameters are automatically escaped
(effect db:query "SELECT * FROM users WHERE age > ? AND name = ?" [18 user-input])
```

**Rust (Safe):**
```rust
// Using the query builder - all values are parameterized
let query = QueryBuilder::new()
    .from("users")
    .where_clause(
        and(
            gt(col("age"), param(Value::Int(18))),
            eq(col("name"), param(Value::String(user_input)))
        )
    )
    .build();

// Execute with proper parameter binding
let (sql, params) = query.to_parameterized_sql()?;
connection.fetch_all(&sql, params).await?;
```

#### 2. Use the Query DSL

The query DSL automatically handles parameterization:

```lisp
(db:from 'users
  (db:where (db:and 
    (db:gt 'age 18)
    (db:eq 'name user-input)))  ; user-input is safely parameterized
  (db:select '(id name email)))
```

#### 3. Schema Definitions

Use type-safe schema definitions that validate data:

```lisp
(define-schema user
  {:id {:type :int :primary-key true}
   :email {:type :string :max-length 255 :pattern "^[^@]+@[^@]+$"}
   :age {:type :int :check "age >= 0 AND age <= 150"}})
```

### ❌ Unsafe Practices (Never Do These)

#### 1. String Concatenation

**NEVER** build queries with string concatenation:

```lisp
;; DANGEROUS - SQL INJECTION VULNERABILITY!
(let ((query (str "SELECT * FROM users WHERE name = '" user-input "'")))
  (effect db:query query []))  ; DON'T DO THIS!
```

#### 2. Raw Query Execution

The `execute_raw_unsafe` method should only be used for DDL statements with NO user input:

```rust
// ONLY use for DDL with no user input
connection.execute_raw_unsafe("CREATE TABLE users (...)").await?;

// NEVER do this with user input!
let query = format!("SELECT * FROM users WHERE name = '{}'", user_input);
connection.execute_raw_unsafe(&query).await?; // DANGER!
```

### 🛡️ Additional Security Features

#### 1. Identifier Escaping

Table and column names are automatically escaped:

```rust
// Column names are escaped to prevent injection
QueryExpr::Column("user's table".to_string())
// Becomes: "user's table" (properly escaped)
```

#### 2. Transaction Isolation

Use transactions to ensure data integrity:

```lisp
(handler
  ((error (lambda (err)
            (effect db:rollback-transaction)
            (logger:error "Transaction failed" {:error err}))))
  (effect db:begin-transaction)
  ;; All queries here are safe and atomic
  (effect db:commit-transaction))
```

#### 3. Connection Security

Always use encrypted connections in production:

```lisp
;; Use SSL/TLS for database connections
(effect db:connect "postgresql://user:pass@host/db?sslmode=require")
```

### 🔍 Security Checklist

Before deploying database code:

- [ ] All user inputs are parameterized (never concatenated)
- [ ] Using query DSL or parameterized queries exclusively
- [ ] Database connections use SSL/TLS
- [ ] Sensitive data is encrypted at rest
- [ ] Database user has minimal required permissions
- [ ] Input validation is performed before queries
- [ ] Error messages don't expose database structure

### 📚 Examples

#### Safe User Authentication

```lisp
(define (authenticate-user email password)
  ;; Email and password are safely parameterized
  (let ((user (effect db:query 
               "SELECT id, password_hash FROM users WHERE email = ?"
               [email])))
    (if (and user (verify-password password (get user :password_hash)))
        (get user :id)
        nil)))
```

#### Safe Search with LIKE

```lisp
(define (search-users search-term)
  ;; Even with LIKE, the parameter is safe
  (effect db:query 
    "SELECT id, name FROM users WHERE name LIKE ?"
    [(str "%" search-term "%")]))  ; Safe!
```

#### Safe Dynamic Queries

```rust
let mut builder = QueryBuilder::new().from("users");

// Add conditions dynamically - all safe!
if let Some(min_age) = filters.min_age {
    builder = builder.where_clause(
        gt(col("age"), param(Value::Int(min_age)))
    );
}

if let Some(city) = filters.city {
    builder = builder.where_clause(
        eq(col("city"), param(Value::String(city)))
    );
}

let query = builder.build();
```

### 🚨 Reporting Security Issues

If you discover a security vulnerability, please email security@claudelang.org with:

1. Description of the vulnerability
2. Steps to reproduce
3. Potential impact
4. Suggested fix (if any)

We take security seriously and will respond within 48 hours.