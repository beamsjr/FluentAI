//! Functional programming utilities

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use crate::vm_bridge::StdlibContext;
use anyhow::{anyhow, Result};

/// Register all functional programming functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Function composition
        StdlibFunction::pure(
            "compose",
            compose,
            2,
            None,
            "Compose functions right-to-left",
        ),
        StdlibFunction::pure("pipe", pipe, 2, None, "Compose functions left-to-right"),
        // Combinators
        StdlibFunction::pure("identity", identity, 1, Some(1), "Identity function"),
        StdlibFunction::pure("const", const_fn, 1, Some(1), "Constant function"),
        StdlibFunction::pure("flip", flip, 1, Some(1), "Flip function arguments"),
        // Partial application
        StdlibFunction::pure("partial", partial, 2, None, "Partial application from left"),
        StdlibFunction::pure(
            "partial-right",
            partial_right,
            2,
            None,
            "Partial application from right",
        ),
        // Currying
        StdlibFunction::pure(
            "curry",
            curry,
            1,
            Some(1),
            "Convert function to curried form",
        ),
        StdlibFunction::pure(
            "uncurry",
            uncurry,
            1,
            Some(1),
            "Convert curried function to uncurried",
        ),
        // Application
        StdlibFunction::pure(
            "apply",
            apply,
            2,
            Some(2),
            "Apply function to list of arguments",
        ),
        StdlibFunction::pure(
            "memoize",
            memoize,
            1,
            Some(1),
            "Create memoized version of function",
        ),
        // List operations
        StdlibFunction::effectful_with_context(
            "map-indexed",
            map_indexed_ctx,
            2,
            Some(2),
            vec![],
            "Map with index",
        ),
        StdlibFunction::effectful_with_context(
            "filter-map",
            filter_map_ctx,
            2,
            Some(2),
            vec![],
            "Filter and map in one pass",
        ),
        StdlibFunction::effectful_with_context(
            "flat-map",
            flat_map_ctx,
            2,
            Some(2),
            vec![],
            "Map and flatten",
        ),
        // Fold variants
        StdlibFunction::pure("fold-right", fold_right, 3, Some(3), "Fold from right"),
        StdlibFunction::pure("scan", scan, 3, Some(3), "Fold with intermediate results"),
        // Predicates
        StdlibFunction::effectful_with_context(
            "all?",
            all_pred_ctx,
            2,
            Some(2),
            vec![],
            "Check if all elements satisfy predicate",
        ),
        StdlibFunction::effectful_with_context(
            "any?",
            any_pred_ctx,
            2,
            Some(2),
            vec![],
            "Check if any element satisfies predicate",
        ),
        StdlibFunction::effectful_with_context(
            "none?",
            none_pred_ctx,
            2,
            Some(2),
            vec![],
            "Check if no elements satisfy predicate",
        ),
        // Iteration
        StdlibFunction::pure(
            "iterate",
            iterate,
            3,
            Some(3),
            "Generate list by iterating function",
        ),
        StdlibFunction::pure("repeat", repeat, 2, Some(2), "Repeat value n times"),
        StdlibFunction::pure(
            "replicate",
            replicate,
            2,
            Some(2),
            "Replicate value n times (alias for repeat)",
        ),
        // Grouping
        StdlibFunction::pure(
            "group-by",
            group_by,
            2,
            Some(2),
            "Group elements by key function",
        ),
        StdlibFunction::pure(
            "chunk",
            chunk,
            2,
            Some(2),
            "Split list into chunks of size n",
        ),
        StdlibFunction::pure(
            "sliding-window",
            sliding_window,
            2,
            Some(2),
            "Create sliding windows of size n",
        ),
    ]);
}

// Function composition
// Note: These require VM integration to properly compose functions

fn compose(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a new composed function
    Err(anyhow!(
        "compose: VM integration required for function composition"
    ))
}

fn pipe(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a new piped function
    Err(anyhow!(
        "pipe: VM integration required for function composition"
    ))
}

// Combinators

fn identity(args: &[Value]) -> Result<Value> {
    Ok(args[0].clone())
}

fn const_fn(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a constant function
    Err(anyhow!(
        "const: VM integration required to create functions"
    ))
}

fn flip(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a flipped function
    Err(anyhow!(
        "flip: VM integration required to transform functions"
    ))
}

// Partial application

fn partial(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a partially applied function
    Err(anyhow!(
        "partial: VM integration required for partial application"
    ))
}

fn partial_right(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a partially applied function
    Err(anyhow!(
        "partial-right: VM integration required for partial application"
    ))
}

// Currying

fn curry(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to curry a function
    Err(anyhow!("curry: VM integration required for currying"))
}

fn uncurry(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to uncurry a function
    Err(anyhow!("uncurry: VM integration required for uncurrying"))
}

// Application

fn apply(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to apply a function
    Err(anyhow!(
        "apply: VM integration required for function application"
    ))
}

fn memoize(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to create a memoized function
    Err(anyhow!("memoize: VM integration required for memoization"))
}

// List operations

fn map_indexed_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("map-indexed: expected function")),
    };

    match &args[1] {
        Value::List(items) => {
            let mut result = Vec::with_capacity(items.len());

            for (index, item) in items.iter().enumerate() {
                // Call the function with index and item
                match context
                    .call_function_with_effects(func, &[Value::Integer(index as i64), item.clone()])
                {
                    Ok(mapped_value) => result.push(mapped_value),
                    Err(e) => return Err(anyhow!("map-indexed: error applying function: {}", e)),
                }
            }

            Ok(Value::List(result))
        }
        _ => Err(anyhow!("map-indexed: expected list")),
    }
}

fn filter_map_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("filter-map: expected function")),
    };

    match &args[1] {
        Value::List(items) => {
            let mut result = Vec::new();

            for item in items {
                // Call the function which should return nil for filtered out items
                match context.call_function_with_effects(func, &[item.clone()]) {
                    Ok(Value::Nil) => {} // Skip nil values
                    Ok(mapped_value) => result.push(mapped_value),
                    Err(e) => return Err(anyhow!("filter-map: error applying function: {}", e)),
                }
            }

            Ok(Value::List(result))
        }
        _ => Err(anyhow!("filter-map: expected list")),
    }
}

fn flat_map_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("flat-map: expected function")),
    };

    match &args[1] {
        Value::List(items) => {
            let mut result = Vec::new();

            for item in items {
                // Call the function which should return a list
                match context.call_function_with_effects(func, &[item.clone()]) {
                    Ok(Value::List(mapped_items)) => {
                        result.extend(mapped_items);
                    }
                    Ok(_) => return Err(anyhow!("flat-map: function must return a list")),
                    Err(e) => return Err(anyhow!("flat-map: error applying function: {}", e)),
                }
            }

            Ok(Value::List(result))
        }
        _ => Err(anyhow!("flat-map: expected list")),
    }
}

// Fold variants

fn fold_right(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to call the folding function
    Err(anyhow!(
        "fold-right: VM integration required for function application"
    ))
}

fn scan(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to call the scanning function
    Err(anyhow!(
        "scan: VM integration required for function application"
    ))
}

// Predicates

fn all_pred_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let pred = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("all?: expected predicate function")),
    };

    match &args[1] {
        Value::List(items) => {
            for item in items {
                match context.call_function_with_effects(pred, &[item.clone()]) {
                    Ok(Value::Boolean(false)) => return Ok(Value::Boolean(false)),
                    Ok(Value::Boolean(true)) => {}
                    Ok(_) => return Err(anyhow!("all?: predicate must return boolean")),
                    Err(e) => return Err(anyhow!("all?: error evaluating predicate: {}", e)),
                }
            }
            Ok(Value::Boolean(true))
        }
        _ => Err(anyhow!("all?: expected list")),
    }
}

fn any_pred_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let pred = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("any?: expected predicate function")),
    };

    match &args[1] {
        Value::List(items) => {
            for item in items {
                match context.call_function_with_effects(pred, &[item.clone()]) {
                    Ok(Value::Boolean(true)) => return Ok(Value::Boolean(true)),
                    Ok(Value::Boolean(false)) => {}
                    Ok(_) => return Err(anyhow!("any?: predicate must return boolean")),
                    Err(e) => return Err(anyhow!("any?: error evaluating predicate: {}", e)),
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(anyhow!("any?: expected list")),
    }
}

fn none_pred_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let pred = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("none?: expected predicate function")),
    };

    match &args[1] {
        Value::List(items) => {
            for item in items {
                match context.call_function_with_effects(pred, &[item.clone()]) {
                    Ok(Value::Boolean(true)) => return Ok(Value::Boolean(false)),
                    Ok(Value::Boolean(false)) => {}
                    Ok(_) => return Err(anyhow!("none?: predicate must return boolean")),
                    Err(e) => return Err(anyhow!("none?: error evaluating predicate: {}", e)),
                }
            }
            Ok(Value::Boolean(true))
        }
        _ => Err(anyhow!("none?: expected list")),
    }
}

// Iteration

fn iterate(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to call the iteration function
    Err(anyhow!(
        "iterate: VM integration required for function application"
    ))
}

fn repeat(args: &[Value]) -> Result<Value> {
    let value = &args[0];
    let count = match &args[1] {
        Value::Integer(n) => *n as usize,
        _ => return Err(anyhow!("repeat: expected integer count")),
    };

    let mut result = Vec::with_capacity(count);
    for _ in 0..count {
        result.push(value.clone());
    }

    Ok(Value::List(result))
}

fn replicate(args: &[Value]) -> Result<Value> {
    // Alias for repeat
    repeat(args)
}

// Grouping

fn group_by(_args: &[Value]) -> Result<Value> {
    // This would need VM integration to call the key function
    Err(anyhow!(
        "group-by: VM integration required for function application"
    ))
}

fn chunk(args: &[Value]) -> Result<Value> {
    let list = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("chunk: expected list")),
    };

    let size = match &args[1] {
        Value::Integer(n) => *n as usize,
        _ => return Err(anyhow!("chunk: expected integer chunk size")),
    };

    if size == 0 {
        return Err(anyhow!("chunk: chunk size must be positive"));
    }

    let mut result = Vec::new();
    let mut current_chunk = Vec::new();

    for item in list {
        current_chunk.push(item.clone());
        if current_chunk.len() == size {
            result.push(Value::List(current_chunk));
            current_chunk = Vec::new();
        }
    }

    if !current_chunk.is_empty() {
        result.push(Value::List(current_chunk));
    }

    Ok(Value::List(result))
}

fn sliding_window(args: &[Value]) -> Result<Value> {
    let list = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("sliding-window: expected list")),
    };

    let size = match &args[1] {
        Value::Integer(n) => *n as usize,
        _ => return Err(anyhow!("sliding-window: expected integer window size")),
    };

    if size == 0 {
        return Err(anyhow!("sliding-window: window size must be positive"));
    }

    if list.len() < size {
        return Ok(Value::List(vec![]));
    }

    let mut result = Vec::new();

    for i in 0..=(list.len() - size) {
        let window: Vec<Value> = list[i..i + size].to_vec();
        result.push(Value::List(window));
    }

    Ok(Value::List(result))
}
