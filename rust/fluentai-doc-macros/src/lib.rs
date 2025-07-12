//! Procedural macros for enforcing documentation standards in FluentAI
//!
//! This crate provides compile-time enforcement of documentation requirements
//! across all FluentAI modules.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn, ItemStruct, Lit, Meta, NestedMeta};

/// Enforces that a function has proper documentation.
///
/// # Example
/// ```
/// use fluentai_doc_macros::documented;
///
/// #[documented(
///     description = "Adds two numbers together",
///     example = "add(2, 3) // returns 5"
/// )]
/// pub fn add(a: i32, b: i32) -> i32 {
///     a + b
/// }
/// ```
#[proc_macro_attribute]
pub fn documented(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let input_fn = parse_macro_input!(input as ItemFn);

    // Extract description and examples from attributes
    let mut description = None;
    let mut examples = Vec::new();

    for arg in args {
        if let NestedMeta::Meta(Meta::NameValue(nv)) = arg {
            if nv.path.is_ident("description") {
                if let Lit::Str(s) = nv.lit {
                    description = Some(s.value());
                }
            } else if nv.path.is_ident("example") {
                if let Lit::Str(s) = nv.lit {
                    examples.push(s.value());
                }
            }
        }
    }

    // Validate that description exists and is non-empty
    let desc = description.unwrap_or_else(|| {
        panic!(
            "Function '{}' must have a non-empty description attribute",
            input_fn.sig.ident
        )
    });

    if desc.trim().is_empty() {
        panic!(
            "Function '{}' must have a non-empty description",
            input_fn.sig.ident
        );
    }

    if desc.split_whitespace().count() < 3 {
        panic!(
            "Function '{}' description is too short. Must be at least 3 words.",
            input_fn.sig.ident
        );
    }

    // Generate the function with added documentation metadata
    let expanded = quote! {
        #[doc = #desc]
        #input_fn
    };

    TokenStream::from(expanded)
}

/// Macro for stdlib functions that enforces documentation and generates registry entries.
///
/// # Example
/// ```
/// use fluentai_doc_macros::stdlib_function;
///
/// #[stdlib_function(
///     name = "length",
///     min_args = 1,
///     max_args = 1,
///     description = "Returns the length of a list",
///     example = "[1, 2, 3].length() // returns 3"
/// )]
/// fn length(args: &[Value]) -> Result<Value> {
///     // implementation
/// }
/// ```
#[proc_macro_attribute]
pub fn stdlib_function(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let input_fn = parse_macro_input!(input as ItemFn);

    // Parse attributes
    let mut name = None;
    let mut min_args = None;
    let mut max_args = None;
    let mut description = None;
    let mut examples = Vec::new();
    let mut effects = Vec::new();

    for arg in args {
        if let NestedMeta::Meta(Meta::NameValue(nv)) = arg {
            match nv.path.get_ident().map(|i| i.to_string()).as_deref() {
                Some("name") => {
                    if let Lit::Str(s) = nv.lit {
                        name = Some(s.value());
                    }
                }
                Some("min_args") => {
                    if let Lit::Int(i) = nv.lit {
                        min_args = Some(i.base10_parse::<usize>().unwrap());
                    }
                }
                Some("max_args") => {
                    if let Lit::Int(i) = nv.lit {
                        max_args = Some(i.base10_parse::<usize>().unwrap());
                    }
                }
                Some("description") => {
                    if let Lit::Str(s) = nv.lit {
                        description = Some(s.value());
                    }
                }
                Some("example") => {
                    if let Lit::Str(s) = nv.lit {
                        examples.push(s.value());
                    }
                }
                Some("effect") => {
                    if let Lit::Str(s) = nv.lit {
                        effects.push(s.value());
                    }
                }
                _ => {}
            }
        }
    }

    // Validate required fields
    let fn_name = name.unwrap_or_else(|| {
        panic!("stdlib_function must have a 'name' attribute");
    });

    let desc = description.unwrap_or_else(|| {
        panic!("stdlib_function '{}' must have a 'description' attribute", fn_name);
    });

    if desc.trim().is_empty() {
        panic!("stdlib_function '{}' must have a non-empty description", fn_name);
    }

    if examples.is_empty() {
        panic!("stdlib_function '{}' must have at least one example", fn_name);
    }

    let min = min_args.unwrap_or_else(|| {
        panic!("stdlib_function '{}' must specify min_args", fn_name);
    });

    // Generate documentation comment
    let doc_comment = format!(
        "{}\n\n# Arguments\n\nMinimum: {}\nMaximum: {}\n\n# Examples\n\n```\n{}\n```",
        desc,
        min,
        max_args.map_or("unlimited".to_string(), |m| m.to_string()),
        examples.join("\n")
    );

    let expanded = quote! {
        #[doc = #doc_comment]
        #input_fn
    };

    TokenStream::from(expanded)
}

/// Enforces documentation on effect handlers.
///
/// # Example
/// ```
/// use fluentai_doc_macros::documented_effect;
///
/// #[documented_effect(
///     effect_type = "IO",
///     operations = ["print", "println", "read_line"],
///     description = "Handles input/output operations"
/// )]
/// pub struct IoHandler {
///     // ...
/// }
/// ```
#[proc_macro_attribute]
pub fn documented_effect(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let input_struct = parse_macro_input!(input as ItemStruct);

    let mut effect_type = None;
    let mut operations = Vec::new();
    let mut description = None;

    for arg in args {
        if let NestedMeta::Meta(Meta::NameValue(nv)) = arg {
            match nv.path.get_ident().map(|i| i.to_string()).as_deref() {
                Some("effect_type") => {
                    if let Lit::Str(s) = nv.lit {
                        effect_type = Some(s.value());
                    }
                }
                Some("description") => {
                    if let Lit::Str(s) = nv.lit {
                        description = Some(s.value());
                    }
                }
                _ => {}
            }
        } else if let NestedMeta::Meta(Meta::List(list)) = arg {
            if list.path.is_ident("operations") {
                for nested in list.nested {
                    if let NestedMeta::Lit(Lit::Str(s)) = nested {
                        operations.push(s.value());
                    }
                }
            }
        }
    }

    let effect = effect_type.unwrap_or_else(|| {
        panic!("documented_effect must have an 'effect_type' attribute");
    });

    let desc = description.unwrap_or_else(|| {
        panic!("documented_effect '{}' must have a 'description' attribute", effect);
    });

    if operations.is_empty() {
        panic!("documented_effect '{}' must list at least one operation", effect);
    }

    let doc_comment = format!(
        "{}\n\n# Effect Type\n\n{}\n\n# Operations\n\n{}",
        desc,
        effect,
        operations.iter().map(|op| format!("- `{}`", op)).collect::<Vec<_>>().join("\n")
    );

    let expanded = quote! {
        #[doc = #doc_comment]
        #input_struct
    };

    TokenStream::from(expanded)
}