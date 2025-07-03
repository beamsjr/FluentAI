//! Database effect types and handlers

use claudelang_effects::EffectHandler;
use claudelang_core::ast::EffectType;
use claudelang_core::value::Value as CoreValue;
use claudelang_core::Result as CoreResult;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{RwLock, Mutex};
use tokio::runtime::Handle;

use crate::connection::{ConnectionPool, Transaction};
use crate::error::DbError;

/// Database effect types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DbEffectType {
    /// Connect to database
    Connect,
    /// Execute a query and return results
    Query,
    /// Execute a non-query command (INSERT, UPDATE, DELETE)
    Execute,
    /// Begin a transaction
    BeginTransaction,
    /// Commit the current transaction
    CommitTransaction,
    /// Rollback the current transaction
    RollbackTransaction,
    /// Get connection statistics
    Stats,
    /// Check if connected
    IsConnected,
    /// Create a prepared statement
    Prepare,
    /// Execute a prepared statement
    ExecutePrepared,
}

/// Database effect handler
pub struct DbHandler {
    connection_pool: Arc<RwLock<Option<ConnectionPool>>>,
    current_transaction: Arc<Mutex<Option<Transaction<'static>>>>,
    transaction_depth: Arc<RwLock<usize>>,
    prepared_statements: Arc<RwLock<HashMap<String, String>>>,
}

impl DbHandler {
    pub fn new() -> Self {
        Self {
            connection_pool: Arc::new(RwLock::new(None)),
            current_transaction: Arc::new(Mutex::new(None)),
            transaction_depth: Arc::new(RwLock::new(0)),
            prepared_statements: Arc::new(RwLock::new(HashMap::new())),
        }
    }
    
    pub fn with_pool(pool: ConnectionPool) -> Self {
        Self {
            connection_pool: Arc::new(RwLock::new(Some(pool))),
            current_transaction: Arc::new(Mutex::new(None)),
            transaction_depth: Arc::new(RwLock::new(0)),
            prepared_statements: Arc::new(RwLock::new(HashMap::new())),
        }
    }
    
    /// Convert CoreValue parameters to VM values
    fn convert_params(&self, params: &CoreValue) -> Result<Vec<claudelang_vm::bytecode::Value>, DbError> {
        match params {
            CoreValue::List(values) => {
                let mut converted = Vec::new();
                for value in values {
                    let vm_value = match value {
                        CoreValue::Nil => claudelang_vm::bytecode::Value::Nil,
                        CoreValue::Boolean(b) => claudelang_vm::bytecode::Value::Bool(*b),
                        CoreValue::Integer(i) => claudelang_vm::bytecode::Value::Int(*i),
                        CoreValue::Float(f) => claudelang_vm::bytecode::Value::Float(*f),
                        CoreValue::String(s) => claudelang_vm::bytecode::Value::String(s.clone()),
                        _ => return Err(DbError::InvalidParameter(format!("Unsupported parameter type: {:?}", value))),
                    };
                    converted.push(vm_value);
                }
                Ok(converted)
            }
            _ => Err(DbError::InvalidParameter("Parameters must be a list".into())),
        }
    }
    
    /// Convert database rows to CoreValue
    fn rows_to_value(&self, rows: Vec<sqlx::any::AnyRow>) -> CoreValue {
        use sqlx::Row;
        use sqlx::Column;
        
        let mut result = Vec::new();
        
        for row in rows {
            let mut row_map = HashMap::new();
            
            // Iterate through columns
            for (idx, column) in row.columns().iter().enumerate() {
                let column_name = column.name().to_string();
                
                // Try to get value as different types
                let value = if let Ok(val) = row.try_get::<Option<i64>, _>(idx) {
                    match val {
                        Some(i) => CoreValue::Integer(i),
                        None => CoreValue::Nil,
                    }
                } else if let Ok(val) = row.try_get::<Option<f64>, _>(idx) {
                    match val {
                        Some(f) => CoreValue::Float(f),
                        None => CoreValue::Nil,
                    }
                } else if let Ok(val) = row.try_get::<Option<String>, _>(idx) {
                    match val {
                        Some(s) => CoreValue::String(s),
                        None => CoreValue::Nil,
                    }
                } else if let Ok(val) = row.try_get::<Option<bool>, _>(idx) {
                    match val {
                        Some(b) => CoreValue::Boolean(b),
                        None => CoreValue::Nil,
                    }
                } else {
                    CoreValue::Nil
                };
                
                row_map.insert(column_name, value);
            }
            
            result.push(CoreValue::Map(row_map));
        }
        
        CoreValue::List(result)
    }
}

impl Clone for DbHandler {
    fn clone(&self) -> Self {
        Self {
            connection_pool: Arc::clone(&self.connection_pool),
            current_transaction: Arc::clone(&self.current_transaction),
            transaction_depth: Arc::clone(&self.transaction_depth),
            prepared_statements: Arc::clone(&self.prepared_statements),
        }
    }
}

impl EffectHandler for DbHandler {
    fn effect_type(&self) -> EffectType {
        // Database operations are IO effects
        EffectType::IO
    }
    
    fn handle_sync(&self, operation: &str, args: &[CoreValue]) -> CoreResult<CoreValue> {
        // Parse the database operation
        let db_op = match operation {
            "db:connect" => DbEffectType::Connect,
            "db:query" => DbEffectType::Query,
            "db:execute" => DbEffectType::Execute,
            "db:begin-transaction" => DbEffectType::BeginTransaction,
            "db:commit-transaction" => DbEffectType::CommitTransaction,
            "db:rollback-transaction" => DbEffectType::RollbackTransaction,
            "db:stats" => DbEffectType::Stats,
            "db:is-connected" => DbEffectType::IsConnected,
            "db:prepare" => DbEffectType::Prepare,
            "db:execute-prepared" => DbEffectType::ExecutePrepared,
            _ => return Err(claudelang_core::error::Error::Runtime(
                format!("Unknown database operation: {}", operation)
            )),
        };
        
        // Most database operations need to run in async context
        let handle = Handle::current();
        let result = handle.block_on(async {
            let value = match db_op {
                DbEffectType::Connect => {
                    // Extract connection string
                    let url = match args.get(0) {
                        Some(CoreValue::String(s)) => s.clone(),
                        _ => return Ok(CoreValue::String("Error: Connect requires connection string".into())),
                    };
                    
                    let config = crate::DbConfig {
                        url,
                        ..Default::default()
                    };
                    
                    let pool = ConnectionPool::new(config);
                    
                    // Test connection
                    match pool.get_connection().await {
                        Ok(_) => {
                            let mut pool_lock = self.connection_pool.write().await;
                            *pool_lock = Some(pool);
                            CoreValue::Boolean(true)
                        }
                        Err(e) => CoreValue::String(format!("Connection failed: {}", e)),
                    }
                }
                
                DbEffectType::Query => {
                    // Extract query string and parameters
                    let query = match args.get(0) {
                        Some(CoreValue::String(s)) => s,
                        _ => return Ok(CoreValue::String("Error: Query requires string argument".into())),
                    };
                    
                    let params = if let Some(p) = args.get(1) {
                        match self.convert_params(p) {
                            Ok(params) => params,
                            Err(e) => return Ok(CoreValue::String(format!("Error: {}", e))),
                        }
                    } else {
                        Vec::new()
                    };
                    
                    // Get connection
                    let pool_lock = self.connection_pool.read().await;
                    if let Some(pool) = pool_lock.as_ref() {
                        match pool.get_connection().await {
                            Ok(conn) => {
                                match conn.fetch_all(query, params).await {
                                    Ok(rows) => self.rows_to_value(rows),
                                    Err(e) => CoreValue::String(format!("Query error: {}", e)),
                                }
                            }
                            Err(e) => CoreValue::String(format!("Connection error: {}", e)),
                        }
                    } else {
                        CoreValue::String("Error: Not connected to database".into())
                    }
                }
                
                DbEffectType::Execute => {
                    // Extract command and parameters
                    let command = match args.get(0) {
                        Some(CoreValue::String(s)) => s,
                        _ => return Ok(CoreValue::String("Error: Execute requires string argument".into())),
                    };
                    
                    let params = if let Some(p) = args.get(1) {
                        match self.convert_params(p) {
                            Ok(params) => params,
                            Err(e) => return Ok(CoreValue::String(format!("Error: {}", e))),
                        }
                    } else {
                        Vec::new()
                    };
                    
                    // Check if we're in a transaction
                    let mut tx_lock = self.current_transaction.lock().await;
                    if let Some(tx) = tx_lock.as_mut() {
                        // Execute within transaction
                        match tx.execute(command, params).await {
                            Ok(rows) => CoreValue::Integer(rows as i64),
                            Err(e) => CoreValue::String(format!("Execute error: {}", e)),
                        }
                    } else {
                        // Execute outside transaction
                        let pool_lock = self.connection_pool.read().await;
                        if let Some(pool) = pool_lock.as_ref() {
                            match pool.get_connection().await {
                                Ok(conn) => {
                                    match conn.execute(command, params).await {
                                        Ok(rows) => CoreValue::Integer(rows as i64),
                                        Err(e) => CoreValue::String(format!("Execute error: {}", e)),
                                    }
                                }
                                Err(e) => CoreValue::String(format!("Connection error: {}", e)),
                            }
                        } else {
                            CoreValue::String("Error: Not connected to database".into())
                        }
                    }
                }
                
                DbEffectType::BeginTransaction => {
                    let pool_lock = self.connection_pool.read().await;
                    if let Some(pool) = pool_lock.as_ref() {
                        match pool.get_connection().await {
                            Ok(conn) => {
                                // Leak the connection to get 'static lifetime
                                // This is safe because we manage the transaction lifetime manually
                                let conn = Box::leak(Box::new(conn));
                                match Transaction::begin(conn).await {
                                    Ok(tx) => {
                                        let mut tx_lock = self.current_transaction.lock().await;
                                        *tx_lock = Some(tx);
                                        let mut depth = self.transaction_depth.write().await;
                                        *depth += 1;
                                        CoreValue::Boolean(true)
                                    }
                                    Err(e) => CoreValue::String(format!("Transaction error: {}", e)),
                                }
                            }
                            Err(e) => CoreValue::String(format!("Connection error: {}", e)),
                        }
                    } else {
                        CoreValue::String("Error: Not connected to database".into())
                    }
                }
                
                DbEffectType::CommitTransaction => {
                    let mut tx_lock = self.current_transaction.lock().await;
                    if let Some(tx) = tx_lock.take() {
                        match tx.commit().await {
                            Ok(()) => {
                                let mut depth = self.transaction_depth.write().await;
                                *depth = depth.saturating_sub(1);
                                CoreValue::Boolean(true)
                            }
                            Err(e) => CoreValue::String(format!("Commit error: {}", e)),
                        }
                    } else {
                        CoreValue::String("Error: No active transaction".into())
                    }
                }
                
                DbEffectType::RollbackTransaction => {
                    let mut tx_lock = self.current_transaction.lock().await;
                    if let Some(tx) = tx_lock.take() {
                        match tx.rollback().await {
                            Ok(()) => {
                                let mut depth = self.transaction_depth.write().await;
                                *depth = depth.saturating_sub(1);
                                CoreValue::Boolean(true)
                            }
                            Err(e) => CoreValue::String(format!("Rollback error: {}", e)),
                        }
                    } else {
                        CoreValue::String("Error: No active transaction".into())
                    }
                }
                
                DbEffectType::Stats => {
                    let mut stats = HashMap::new();
                    let pool_lock = self.connection_pool.read().await;
                    stats.insert("connected".to_string(), CoreValue::Boolean(pool_lock.is_some()));
                    
                    let depth = self.transaction_depth.read().await;
                    stats.insert("transaction_depth".to_string(), CoreValue::Integer(*depth as i64));
                    
                    let stmts = self.prepared_statements.read().await;
                    stats.insert("prepared_statements".to_string(), CoreValue::Integer(stmts.len() as i64));
                    
                    CoreValue::Map(stats)
                }
                
                DbEffectType::IsConnected => {
                    let pool_lock = self.connection_pool.read().await;
                    if let Some(pool) = pool_lock.as_ref() {
                        match pool.get_connection().await {
                            Ok(conn) => CoreValue::Boolean(conn.is_connected().await),
                            Err(_) => CoreValue::Boolean(false),
                        }
                    } else {
                        CoreValue::Boolean(false)
                    }
                }
                
                DbEffectType::Prepare => {
                    let stmt_id = match args.get(0) {
                        Some(CoreValue::String(s)) => s.clone(),
                        _ => return Ok(CoreValue::String("Error: Prepare requires statement ID".into())),
                    };
                    
                    let query = match args.get(1) {
                        Some(CoreValue::String(s)) => s.clone(),
                        _ => return Ok(CoreValue::String("Error: Prepare requires query string".into())),
                    };
                    
                    // Store the prepared statement
                    let mut stmts = self.prepared_statements.write().await;
                    stmts.insert(stmt_id.clone(), query);
                    
                    CoreValue::String(stmt_id)
                }
                
                DbEffectType::ExecutePrepared => {
                    let stmt_id = match args.get(0) {
                        Some(CoreValue::String(s)) => s,
                        _ => return Ok(CoreValue::String("Error: ExecutePrepared requires statement ID".into())),
                    };
                    
                    let params = if let Some(p) = args.get(1) {
                        match self.convert_params(p) {
                            Ok(params) => params,
                            Err(e) => return Ok(CoreValue::String(format!("Error: {}", e))),
                        }
                    } else {
                        Vec::new()
                    };
                    
                    // Get the prepared statement
                    let stmts = self.prepared_statements.read().await;
                    if let Some(query) = stmts.get(stmt_id) {
                        let query = query.clone();
                        drop(stmts); // Release lock before executing
                        
                        // Execute the query
                        let pool_lock = self.connection_pool.read().await;
                        if let Some(pool) = pool_lock.as_ref() {
                            match pool.get_connection().await {
                                Ok(conn) => {
                                    match conn.fetch_all(&query, params).await {
                                        Ok(rows) => self.rows_to_value(rows),
                                        Err(e) => CoreValue::String(format!("Query error: {}", e)),
                                    }
                                }
                                Err(e) => CoreValue::String(format!("Connection error: {}", e)),
                            }
                        } else {
                            CoreValue::String("Error: Not connected to database".into())
                        }
                    } else {
                        CoreValue::String(format!("Error: No prepared statement with ID: {}", stmt_id))
                    }
                }
            };
            Ok(value)
        });
        
        result
    }
}

impl Default for DbHandler {
    fn default() -> Self {
        Self::new()
    }
}