//! Method call adapters for list operations
//!
//! These adapters handle the argument reordering needed when calling
//! functions as methods. For example, `list.map(fn)` becomes `map(list, fn)`
//! but the core functions expect `map(fn, list)`.

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use crate::vm_bridge::StdlibContext;
use anyhow::{anyhow, Result};
use fluentai_core::ast::EffectType;

/// Register method-style adapters for list operations and other types
pub fn register(registry: &mut StdlibRegistry) {
    // These are registered with the same names as the methods
    // but handle the argument reordering
    registry.register_all(vec![
        // Print method for Printable tagged values
        StdlibFunction::effectful_with_context(
            "print",
            print_method,
            1,
            Some(1),
            vec![EffectType::IO],
            "Method adapter for print on Printable values",
        ),
        // Note: These expect (list, fn) and call the core functions with (fn, list)
        StdlibFunction::effectful_with_context(
            "map",
            map_method,
            2,
            Some(2),
            vec![],
            "Method adapter for map",
        ),
        StdlibFunction::effectful_with_context(
            "filter", 
            filter_method,
            2,
            Some(2),
            vec![],
            "Method adapter for filter",
        ),
        StdlibFunction::effectful_with_context(
            "reduce",
            reduce_method,
            3,
            Some(3),
            vec![],
            "Method adapter for reduce",
        ),
        StdlibFunction::pure(
            "length",
            length_method,
            1,
            Some(1),
            "Method adapter for length",
        ),
        StdlibFunction::pure(
            "append",
            append_method,
            2,
            Some(2),
            "Method adapter for append",
        ),
        StdlibFunction::effectful_with_context(
            "for_each",
            for_each_method,
            2,
            Some(2),
            vec![],
            "Method adapter for for_each",
        ),
    ]);
}

// Method adapters that reorder arguments

fn print_method(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    // Method call: printable.print() -> args are [printable]
    // This is for Printable tagged values that have a print method
    if args.len() != 1 {
        return Err(anyhow!("print method expects exactly 1 argument (self)"));
    }
    
    // Check if it's a Printable tagged value
    match &args[0] {
        Value::Tagged { tag, values: _ } => {
            if tag != "Printable" {
                return Err(anyhow!("print method can only be called on Printable values"));
            }
        }
        _ => return Err(anyhow!("print method can only be called on Printable values")),
    }
    
    // Call the IO effect directly
    let effect_context = context.effect_context();
    effect_context
        .perform_sync(EffectType::IO, "print", args)
        .map_err(|e| anyhow!("IO effect error: {}", e))
}

fn map_method(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    // Method call: list.map(fn) -> args are [list, fn]
    // Core function expects: map(fn, list)
    if args.len() != 2 {
        return Err(anyhow!("map expects exactly 2 arguments"));
    }
    
    // Get the core map function and call it with reordered args
    let core_args = vec![args[1].clone(), args[0].clone()];
    
    // Use the core map implementation directly
    crate::core::map_ctx(context, &core_args)
}

fn filter_method(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    // Method call: list.filter(fn) -> args are [list, fn]
    // Core function expects: filter(fn, list)
    if args.len() != 2 {
        return Err(anyhow!("filter expects exactly 2 arguments"));
    }
    
    let core_args = vec![args[1].clone(), args[0].clone()];
    crate::core::filter_ctx(context, &core_args)
}

fn reduce_method(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    // Method call: list.reduce(init, fn) -> args are [list, init, fn]
    // Core function expects: fold(fn, init, list)
    if args.len() != 3 {
        return Err(anyhow!("reduce expects exactly 3 arguments"));
    }
    
    let core_args = vec![args[2].clone(), args[1].clone(), args[0].clone()];
    crate::core::fold_ctx(context, &core_args)
}

fn length_method(args: &[Value]) -> Result<Value> {
    // Method call: list.length() -> args are [list]
    // Core function expects: length(list)
    crate::core::length(args)
}

fn append_method(args: &[Value]) -> Result<Value> {
    // Method call: list.append(item) -> args are [list, item]
    // Core function expects: append(list, item)
    crate::core::append(args)
}

fn for_each_method(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    // Method call: collection.for_each(fn) -> args are [collection, fn]
    // Core function expects: for-each(fn, collection)
    if args.len() != 2 {
        return Err(anyhow!("for_each expects exactly 2 arguments"));
    }
    
    let core_args = vec![args[1].clone(), args[0].clone()];
    crate::core::for_each_ctx(context, &core_args)
}