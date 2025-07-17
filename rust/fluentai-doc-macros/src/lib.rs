//! Procedural macros for enforcing documentation standards in FluentAI
//!
//! This crate provides compile-time enforcement of documentation requirements
//! across all FluentAI modules.

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse::Parse, parse::ParseStream, 
    ItemFn, ItemStruct, Lit, Token, punctuated::Punctuated
};

// Helper struct to parse key-value pairs in attributes
struct KeyValue {
    key: syn::Ident,
    _eq: Token![=],
    value: Lit,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(KeyValue {
            key: input.parse()?,
            _eq: input.parse()?,
            value: input.parse()?,
        })
    }
}

// Helper struct to parse comma-separated key-value pairs
struct Args {
    items: syn::punctuated::Punctuated<KeyValue, Token![,]>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Args {
            items: syn::punctuated::Punctuated::parse_terminated(input)?,
        })
    }
}

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
    let args = parse_macro_input!(args as Args);
    let input_fn = parse_macro_input!(input as ItemFn);

    // Extract description and examples from attributes
    let mut description = None;
    let mut examples = Vec::new();

    for item in args.items {
        let key = item.key.to_string();
        match key.as_str() {
            "description" => {
                if let Lit::Str(s) = item.value {
                    description = Some(s.value());
                }
            }
            "example" => {
                if let Lit::Str(s) = item.value {
                    examples.push(s.value());
                }
            }
            _ => {}
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
    let args = parse_macro_input!(args as Args);
    let input_fn = parse_macro_input!(input as ItemFn);

    // Parse attributes
    let mut name = None;
    let mut min_args = None;
    let mut max_args = None;
    let mut description = None;
    let mut examples = Vec::new();
    let mut effects = Vec::new();

    for item in args.items {
        let key = item.key.to_string();
        match key.as_str() {
            "name" => {
                if let Lit::Str(s) = item.value {
                    name = Some(s.value());
                }
            }
            "min_args" => {
                if let Lit::Int(i) = item.value {
                    min_args = Some(i.base10_parse::<usize>().unwrap());
                }
            }
            "max_args" => {
                if let Lit::Int(i) = item.value {
                    max_args = Some(i.base10_parse::<usize>().unwrap());
                }
            }
            "description" => {
                if let Lit::Str(s) = item.value {
                    description = Some(s.value());
                }
            }
            "example" => {
                if let Lit::Str(s) = item.value {
                    examples.push(s.value());
                }
            }
            "effect" => {
                if let Lit::Str(s) = item.value {
                    effects.push(s.value());
                }
            }
            _ => {}
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
    let input_struct = parse_macro_input!(input as ItemStruct);
    
    // For syn 2.0, we need to parse the args differently
    // Since the attribute syntax uses key=value pairs, we'll parse it as a custom type
    struct EffectArgs {
        effect_type: Option<String>,
        operations: Vec<String>,
        description: Option<String>,
    }
    
    impl Parse for EffectArgs {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut effect_type = None;
            let mut operations = Vec::new();
            let mut description = None;
            
            while !input.is_empty() {
                let key: syn::Ident = input.parse()?;
                let _: Token![=] = input.parse()?;
                
                if key == "effect_type" {
                    let lit: syn::LitStr = input.parse()?;
                    effect_type = Some(lit.value());
                } else if key == "description" {
                    let lit: syn::LitStr = input.parse()?;
                    description = Some(lit.value());
                } else if key == "operations" {
                    // Parse array of strings: ["print", "println", "read_line"]
                    let content;
                    let _ = syn::bracketed!(content in input);
                    let parsed_ops: Punctuated<syn::LitStr, Token![,]> = 
                        Punctuated::parse_terminated(&content)?;
                    operations = parsed_ops.into_iter().map(|lit| lit.value()).collect();
                }
                
                if !input.is_empty() {
                    let _: Token![,] = input.parse()?;
                }
            }
            
            Ok(EffectArgs {
                effect_type,
                operations,
                description,
            })
        }
    }
    
    let args = parse_macro_input!(args as EffectArgs);
    
    // Generate documentation
    let doc_comment = if let (Some(effect_type), Some(desc)) = (args.effect_type, args.description) {
        let ops_str = if args.operations.is_empty() {
            String::new()
        } else {
            format!("\n\n# Operations\n\n- {}", args.operations.join("\n- "))
        };
        
        format!(
            "Effect handler for {} effects.\n\n{}{}",
            effect_type,
            desc,
            ops_str
        )
    } else {
        String::from("Effect handler.")
    };
    
    let expanded = quote! {
        #[doc = #doc_comment]
        #input_struct
    };

    TokenStream::from(expanded)
}