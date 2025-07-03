//! Functional query DSL for ClaudeLang

use claudelang_vm::bytecode::Value;
use crate::error::DbResult;

/// Parameter value for safe query building
#[derive(Debug, Clone)]
pub struct Parameter {
    pub index: usize,
    pub value: Value,
}

/// Query expression types
#[derive(Debug, Clone)]
pub enum QueryExpr {
    /// Table reference
    Table(String),
    /// Column reference
    Column(String),
    /// Parameter placeholder (?, $1, :name etc)
    Parameter(Parameter),
    /// Binary operation
    BinOp {
        op: BinOp,
        left: Box<QueryExpr>,
        right: Box<QueryExpr>,
    },
    /// Unary operation
    UnaryOp {
        op: UnaryOp,
        expr: Box<QueryExpr>,
    },
    /// Function call
    Function {
        name: String,
        args: Vec<QueryExpr>,
    },
    /// Aggregate function
    Aggregate {
        func: AggregateFunc,
        expr: Box<QueryExpr>,
    },
    /// Case expression
    Case {
        conditions: Vec<(QueryExpr, QueryExpr)>,
        else_expr: Option<Box<QueryExpr>>,
    },
    /// Subquery
    Subquery(Box<Query>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Comparison
    Eq, Ne, Lt, Le, Gt, Ge,
    // Logical
    And, Or,
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // String
    Like, NotLike,
    // Set
    In, NotIn,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    IsNull,
    IsNotNull,
}

/// Aggregate functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggregateFunc {
    Count,
    Sum,
    Avg,
    Min,
    Max,
}

/// Join types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JoinType {
    Inner,
    Left,
    Right,
    Full,
}

/// Order direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrderDir {
    Asc,
    Desc,
}

/// A functional query representation
#[derive(Debug, Clone)]
pub struct Query {
    /// SELECT expressions
    pub select: Vec<(QueryExpr, Option<String>)>, // (expr, alias)
    /// FROM clause
    pub from: Option<QueryExpr>,
    /// JOIN clauses
    pub joins: Vec<(JoinType, QueryExpr, QueryExpr)>, // (type, table, condition)
    /// WHERE clause
    pub where_clause: Option<QueryExpr>,
    /// GROUP BY expressions
    pub group_by: Vec<QueryExpr>,
    /// HAVING clause
    pub having: Option<QueryExpr>,
    /// ORDER BY expressions
    pub order_by: Vec<(QueryExpr, OrderDir)>,
    /// LIMIT clause
    pub limit: Option<usize>,
    /// OFFSET clause
    pub offset: Option<usize>,
}

impl Query {
    pub fn new() -> Self {
        Self {
            select: Vec::new(),
            from: None,
            joins: Vec::new(),
            where_clause: None,
            group_by: Vec::new(),
            having: None,
            order_by: Vec::new(),
            limit: None,
            offset: None,
        }
    }
    
    /// Convert to parameterized SQL string
    pub fn to_parameterized_sql(&mut self) -> DbResult<(String, Vec<Value>)> {
        let mut builder = SqlBuilder::new();
        let sql = builder.build_sql(self)?;
        Ok((sql, builder.parameters))
    }
}

/// Helper struct to build SQL and collect parameters
struct SqlBuilder {
    parameters: Vec<Value>,
}

impl SqlBuilder {
    fn new() -> Self {
        Self {
            parameters: Vec::new(),
        }
    }
    
    /// Build SQL with parameter placeholders
    fn build_sql(&mut self, query: &Query) -> DbResult<String> {
        let mut sql = String::new();
        
        // SELECT clause
        if query.select.is_empty() {
            sql.push_str("SELECT *");
        } else {
            sql.push_str("SELECT ");
            for (i, (expr, alias)) in query.select.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&self.expr_to_sql(expr)?);
                if let Some(alias) = alias {
                    sql.push_str(" AS ");
                    sql.push_str(alias);
                }
            }
        }
        
        // FROM clause
        if let Some(from) = &query.from {
            sql.push_str(" FROM ");
            sql.push_str(&self.expr_to_sql(from)?);
        }
        
        // JOIN clauses
        for (join_type, table, condition) in &query.joins {
            match join_type {
                JoinType::Inner => sql.push_str(" INNER JOIN "),
                JoinType::Left => sql.push_str(" LEFT JOIN "),
                JoinType::Right => sql.push_str(" RIGHT JOIN "),
                JoinType::Full => sql.push_str(" FULL JOIN "),
            }
            sql.push_str(&self.expr_to_sql(table)?);
            sql.push_str(" ON ");
            sql.push_str(&self.expr_to_sql(condition)?);
        }
        
        // WHERE clause
        if let Some(where_clause) = &query.where_clause {
            sql.push_str(" WHERE ");
            sql.push_str(&self.expr_to_sql(where_clause)?);
        }
        
        // GROUP BY clause
        if !query.group_by.is_empty() {
            sql.push_str(" GROUP BY ");
            for (i, expr) in query.group_by.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&self.expr_to_sql(expr)?);
            }
        }
        
        // HAVING clause
        if let Some(having) = &query.having {
            sql.push_str(" HAVING ");
            sql.push_str(&self.expr_to_sql(having)?);
        }
        
        // ORDER BY clause
        if !query.order_by.is_empty() {
            sql.push_str(" ORDER BY ");
            for (i, (expr, dir)) in query.order_by.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&self.expr_to_sql(expr)?);
                match dir {
                    OrderDir::Asc => sql.push_str(" ASC"),
                    OrderDir::Desc => sql.push_str(" DESC"),
                }
            }
        }
        
        // LIMIT clause
        if let Some(limit) = query.limit {
            sql.push_str(&format!(" LIMIT {}", limit));
        }
        
        // OFFSET clause
        if let Some(offset) = query.offset {
            sql.push_str(&format!(" OFFSET {}", offset));
        }
        
        Ok(sql)
    }
    
    /// Convert expression to SQL with parameter placeholders
    fn expr_to_sql(&mut self, expr: &QueryExpr) -> DbResult<String> {
        match expr {
            QueryExpr::Table(name) => Ok(escape_identifier(name)),
            QueryExpr::Column(name) => Ok(escape_identifier(name)),
            QueryExpr::Parameter(param) => {
                self.parameters.push(param.value.clone());
                // Use ? for parameter placeholder (works with most databases)
                // Could be adapted for specific databases ($1, $2 for Postgres, etc)
                Ok("?".to_string())
            }
            QueryExpr::BinOp { op, left, right } => {
                Ok(format!(
                    "({} {} {})",
                    self.expr_to_sql(left)?,
                    binop_to_sql(*op),
                    self.expr_to_sql(right)?
                ))
            }
            QueryExpr::UnaryOp { op, expr } => {
                match op {
                    UnaryOp::Not => Ok(format!("NOT ({})", self.expr_to_sql(expr)?)),
                    UnaryOp::IsNull => Ok(format!("({} IS NULL)", self.expr_to_sql(expr)?)),
                    UnaryOp::IsNotNull => Ok(format!("({} IS NOT NULL)", self.expr_to_sql(expr)?)),
                }
            }
            QueryExpr::Function { name, args } => {
                let args_sql: Result<Vec<_>, _> = args.iter()
                    .map(|arg| self.expr_to_sql(arg))
                    .collect();
                Ok(format!("{}({})", name, args_sql?.join(", ")))
            }
            QueryExpr::Aggregate { func, expr } => {
                Ok(format!("{}({})", aggregate_to_sql(*func), self.expr_to_sql(expr)?))
            }
            QueryExpr::Case { conditions, else_expr } => {
                let mut sql = String::from("CASE");
                for (cond, result) in conditions {
                    sql.push_str(&format!(
                        " WHEN {} THEN {}",
                        self.expr_to_sql(cond)?,
                        self.expr_to_sql(result)?
                    ));
                }
                if let Some(else_expr) = else_expr {
                    sql.push_str(&format!(" ELSE {}", self.expr_to_sql(else_expr)?));
                }
                sql.push_str(" END");
                Ok(sql)
            }
            QueryExpr::Subquery(query) => {
                let mut subquery = (**query).clone();
                let (subsql, subparams) = subquery.to_parameterized_sql()?;
                // Add subquery parameters to our parameter list
                self.parameters.extend(subparams);
                Ok(format!("({})", subsql))
            }
        }
    }
}

/// Escape SQL identifier to prevent injection
fn escape_identifier(name: &str) -> String {
    // Simple escaping - in production, use database-specific escaping
    format!("\"{}\"", name.replace('"', "\"\""))
}

fn binop_to_sql(op: BinOp) -> &'static str {
    match op {
        BinOp::Eq => "=",
        BinOp::Ne => "!=",
        BinOp::Lt => "<",
        BinOp::Le => "<=",
        BinOp::Gt => ">",
        BinOp::Ge => ">=",
        BinOp::And => "AND",
        BinOp::Or => "OR",
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Mod => "%",
        BinOp::Like => "LIKE",
        BinOp::NotLike => "NOT LIKE",
        BinOp::In => "IN",
        BinOp::NotIn => "NOT IN",
    }
}

fn aggregate_to_sql(func: AggregateFunc) -> &'static str {
    match func {
        AggregateFunc::Count => "COUNT",
        AggregateFunc::Sum => "SUM",
        AggregateFunc::Avg => "AVG",
        AggregateFunc::Min => "MIN",
        AggregateFunc::Max => "MAX",
    }
}

/// Query builder for fluent API
pub struct QueryBuilder {
    query: Query,
    param_counter: usize,
}

impl QueryBuilder {
    pub fn new() -> Self {
        Self {
            query: Query::new(),
            param_counter: 0,
        }
    }
    
    pub fn next_param(&mut self, value: Value) -> QueryExpr {
        let param = Parameter {
            index: self.param_counter,
            value,
        };
        self.param_counter += 1;
        QueryExpr::Parameter(param)
    }
    
    pub fn from(mut self, table: &str) -> Self {
        self.query.from = Some(QueryExpr::Table(table.to_string()));
        self
    }
    
    pub fn select(mut self, columns: Vec<&str>) -> Self {
        self.query.select = columns.into_iter()
            .map(|col| (QueryExpr::Column(col.to_string()), None))
            .collect();
        self
    }
    
    pub fn select_expr(mut self, expr: QueryExpr, alias: Option<String>) -> Self {
        self.query.select.push((expr, alias));
        self
    }
    
    pub fn where_clause(mut self, expr: QueryExpr) -> Self {
        self.query.where_clause = Some(expr);
        self
    }
    
    pub fn group_by(mut self, columns: Vec<&str>) -> Self {
        self.query.group_by = columns.into_iter()
            .map(|col| QueryExpr::Column(col.to_string()))
            .collect();
        self
    }
    
    pub fn order_by(mut self, column: &str, dir: OrderDir) -> Self {
        self.query.order_by.push((QueryExpr::Column(column.to_string()), dir));
        self
    }
    
    pub fn limit(mut self, limit: usize) -> Self {
        self.query.limit = Some(limit);
        self
    }
    
    pub fn offset(mut self, offset: usize) -> Self {
        self.query.offset = Some(offset);
        self
    }
    
    pub fn build(self) -> Query {
        self.query
    }
}

/// Helper functions for building expressions safely
pub fn col(name: &str) -> QueryExpr {
    QueryExpr::Column(name.to_string())
}

pub fn param(value: Value) -> Parameter {
    Parameter {
        index: 0, // Will be set by builder
        value,
    }
}

pub fn eq(left: QueryExpr, right: QueryExpr) -> QueryExpr {
    QueryExpr::BinOp {
        op: BinOp::Eq,
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn gt(left: QueryExpr, right: QueryExpr) -> QueryExpr {
    QueryExpr::BinOp {
        op: BinOp::Gt,
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn and(left: QueryExpr, right: QueryExpr) -> QueryExpr {
    QueryExpr::BinOp {
        op: BinOp::And,
        left: Box::new(left),
        right: Box::new(right),
    }
}

/// Safe query builder example
pub fn safe_user_query(age_threshold: i64, active: bool) -> Query {
    QueryBuilder::new()
        .from("users")
        .select(vec!["id", "name", "email"])
        .where_clause(
            and(
                gt(col("age"), QueryExpr::Parameter(Parameter {
                    index: 0,
                    value: Value::Int(age_threshold),
                })),
                eq(col("active"), QueryExpr::Parameter(Parameter {
                    index: 1,
                    value: Value::Bool(active),
                }))
            )
        )
        .order_by("created_at", OrderDir::Desc)
        .limit(100)
        .build()
}
#[cfg(test)]
mod tests;
