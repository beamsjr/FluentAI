# FluentAi Database Module

A functional database effect system for FluentAi that provides type-safe, composable database operations while maintaining functional purity.

> **🛡️ Security Note**: This module uses parameterized queries exclusively to prevent SQL injection. See [SECURITY.md](SECURITY.md) for security best practices.

## Features

- **Effect-based database operations** - All database operations are tracked as effects
- **Functional query DSL** - Build queries using pure functional combinators
- **Type-safe schema definition** - Define schemas with compile-time validation
- **Transaction support** - ACID transactions with automatic rollback
- **Connection pooling** - Efficient connection management with DI integration
- **Multi-database support** - Works with PostgreSQL, MySQL, and SQLite

## Usage

### Basic Queries

```clojure
; Connect to database
(effect db:connect "postgresql://localhost/mydb")

; Execute a query
(effect db:query "SELECT * FROM users WHERE age > ?" [18])

; Execute a command
(effect db:execute "INSERT INTO users (name, email) VALUES (?, ?)" ["Alice" "alice@example.com"])
```

### Functional Query DSL

```clojure
; Build queries functionally
(db:from 'users
  (db:where (db:and
    (db:gt 'age 18)
    (db:eq 'active true)))
  (db:select '(id name email))
  (db:order-by 'created_at :desc)
  (db:limit 10))
```

### Schema Definition

```clojure
; Define a schema
(define-schema user
  {:id {:type :int :primary-key true}
   :name {:type :string :max-length 100 :not-null true}
   :email {:type :string :unique true :not-null true}
   :age {:type :int :check "age >= 0"}
   :created_at {:type :timestamp :default :current-timestamp}})
```

### Transactions

```clojure
; Use transactions for atomic operations
(effect db:begin-transaction)
(try
  (effect db:execute "INSERT INTO accounts (id, balance) VALUES (?, ?)" [1 1000])
  (effect db:execute "INSERT INTO accounts (id, balance) VALUES (?, ?)" [2 1000])
  (effect db:execute "UPDATE accounts SET balance = balance - 100 WHERE id = 1")
  (effect db:execute "UPDATE accounts SET balance = balance + 100 WHERE id = 2")
  (effect db:commit-transaction)
  (catch e
    (effect db:rollback-transaction)
    (error:raise "transaction-failed" e)))
```

### Prepared Statements

```clojure
; Prepare a statement for repeated use
(effect db:prepare "find-user" "SELECT * FROM users WHERE email = ?")

; Execute prepared statement
(effect db:execute-prepared "find-user" ["alice@example.com"])
```

## Architecture

The database module is built on several key components:

1. **Effect System** - Database operations are effects that can be tracked and controlled
2. **Query DSL** - A functional API for building SQL queries
3. **Schema System** - Type-safe schema definitions with migration support
4. **Connection Pool** - Efficient connection management integrated with DI
5. **Transaction Manager** - ACID transaction support with automatic cleanup

## Integration with FluentAi

The database module integrates seamlessly with FluentAi's:

- **Effect System** - All database operations are tracked as IO effects
- **Type System** - Schema definitions are type-checked at compile time
- **DI Container** - Connection pools are managed through dependency injection
- **Error Handling** - Database errors integrate with FluentAi's error effects

## Performance

- Connection pooling minimizes connection overhead
- Prepared statements reduce query parsing time
- Functional query building enables query optimization
- Effect tracking allows for batching and caching

## Testing

The functional approach makes testing straightforward:

```clojure
; Mock database effects for testing
(with-mock-db-handler
  (lambda ()
    ; Your test code here
    ; Database effects will be captured instead of executed
    ))
```

## Future Enhancements

- Query result mapping to FluentAi records
- Advanced query optimization
- Database migration management
- Streaming query results
- Connection health monitoring
- Query performance analytics