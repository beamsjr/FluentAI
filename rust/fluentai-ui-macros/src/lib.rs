// FluentAI UI Macros - Procedural macros for reactive UI development

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, LitStr, Expr, Token, parse::{Parse, ParseStream}};

/// The `ui!` macro provides a declarative syntax for creating reactive UIs
/// 
/// # Example
/// ```ignore
/// ui! {
///     Stack(direction: .vertical, spacing: 10) {
///         Text("Hello, World!")
///             .font_size(24)
///             .color(Color::blue())
///         
///         Button("Click me") {
///             on_click: || println!("Button clicked!")
///         }
///         
///         if show_details {
///             Text("Additional details...")
///         }
///         
///         for item in items {
///             ListItem(item)
///         }
///     }
/// }
/// ```
#[proc_macro]
pub fn ui(input: TokenStream) -> TokenStream {
    let ui_tree = parse_macro_input!(input as UiTree);
    let expanded = expand_ui_tree(&ui_tree);
    TokenStream::from(expanded)
}

/// The `reactive!` macro creates reactive state variables
/// 
/// # Example
/// ```ignore
/// reactive! {
///     count: i32 = 0,
///     name: String = "User".to_string(),
///     items: Vec<Item> = vec![]
/// }
/// ```
#[proc_macro]
pub fn reactive(input: TokenStream) -> TokenStream {
    let reactive_vars = parse_macro_input!(input as ReactiveVars);
    let expanded = expand_reactive_vars(&reactive_vars);
    TokenStream::from(expanded)
}

/// The `computed!` macro creates computed properties that automatically update
/// 
/// # Example
/// ```ignore
/// computed! {
///     total = items.iter().map(|i| i.price).sum(),
///     display_name = format!("{} ({})", name, count)
/// }
/// ```
#[proc_macro]
pub fn computed(input: TokenStream) -> TokenStream {
    let computed_props = parse_macro_input!(input as ComputedProps);
    let expanded = expand_computed_props(&computed_props);
    TokenStream::from(expanded)
}

/// The `effect!` macro creates side effects that run when dependencies change
/// 
/// # Example
/// ```ignore
/// effect! {
///     // Runs whenever count changes
///     [count] => {
///         println!("Count changed to: {}", count);
///     }
/// }
/// ```
#[proc_macro]
pub fn effect(input: TokenStream) -> TokenStream {
    let effect_def = parse_macro_input!(input as EffectDef);
    let expanded = expand_effect(&effect_def);
    TokenStream::from(expanded)
}

// AST structures for parsing

struct UiTree {
    root: UiNode,
}

struct UiNode {
    component: syn::Ident,
    props: Vec<UiProp>,
    children: Vec<UiChild>,
}

struct UiProp {
    name: syn::Ident,
    value: Expr,
}

enum UiChild {
    Node(UiNode),
    Text(LitStr),
    Expression(Expr),
    Conditional { condition: Expr, body: Box<UiNode> },
    Loop { var: syn::Ident, iter: Expr, body: Box<UiNode> },
}

struct ReactiveVars {
    vars: Vec<ReactiveVar>,
}

struct ReactiveVar {
    name: syn::Ident,
    ty: syn::Type,
    init: Expr,
}

struct ComputedProps {
    props: Vec<ComputedProp>,
}

struct ComputedProp {
    name: syn::Ident,
    expr: Expr,
}

struct EffectDef {
    deps: Vec<syn::Ident>,
    body: syn::Block,
}

// Parsing implementations

impl Parse for UiTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(UiTree {
            root: input.parse()?,
        })
    }
}

impl Parse for UiNode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let component: syn::Ident = input.parse()?;
        
        // Parse props if present
        let mut props = Vec::new();
        if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            
            while !content.is_empty() {
                let name: syn::Ident = content.parse()?;
                content.parse::<Token![:]>()?;
                let value: Expr = content.parse()?;
                props.push(UiProp { name, value });
                
                if !content.is_empty() {
                    content.parse::<Token![,]>()?;
                }
            }
        }
        
        // Parse children if present
        let mut children = Vec::new();
        if input.peek(syn::token::Brace) {
            let content;
            syn::braced!(content in input);
            
            while !content.is_empty() {
                children.push(content.parse()?);
            }
        }
        
        Ok(UiNode {
            component,
            props,
            children,
        })
    }
}

impl Parse for UiChild {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            Ok(UiChild::Text(input.parse()?))
        } else if input.peek(Token![if]) {
            input.parse::<Token![if]>()?;
            let condition: Expr = input.parse()?;
            let content;
            syn::braced!(content in input);
            let body = Box::new(content.parse()?);
            Ok(UiChild::Conditional { condition, body })
        } else if input.peek(Token![for]) {
            input.parse::<Token![for]>()?;
            let var: syn::Ident = input.parse()?;
            input.parse::<Token![in]>()?;
            let iter: Expr = input.parse()?;
            let content;
            syn::braced!(content in input);
            let body = Box::new(content.parse()?);
            Ok(UiChild::Loop { var, iter, body })
        } else if input.peek(syn::Ident) {
            Ok(UiChild::Node(input.parse()?))
        } else {
            Ok(UiChild::Expression(input.parse()?))
        }
    }
}

impl Parse for ReactiveVars {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut vars = Vec::new();
        
        while !input.is_empty() {
            let name: syn::Ident = input.parse()?;
            input.parse::<Token![:]>()?;
            let ty: syn::Type = input.parse()?;
            input.parse::<Token![=]>()?;
            let init: Expr = input.parse()?;
            
            vars.push(ReactiveVar { name, ty, init });
            
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        
        Ok(ReactiveVars { vars })
    }
}

impl Parse for ComputedProps {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut props = Vec::new();
        
        while !input.is_empty() {
            let name: syn::Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let expr: Expr = input.parse()?;
            
            props.push(ComputedProp { name, expr });
            
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        
        Ok(ComputedProps { props })
    }
}

impl Parse for EffectDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let deps_content;
        syn::bracketed!(deps_content in input);
        
        let mut deps = Vec::new();
        while !deps_content.is_empty() {
            deps.push(deps_content.parse()?);
            if !deps_content.is_empty() {
                deps_content.parse::<Token![,]>()?;
            }
        }
        
        input.parse::<Token![=>]>()?;
        let body: syn::Block = input.parse()?;
        
        Ok(EffectDef { deps, body })
    }
}

// Expansion functions

fn expand_ui_tree(tree: &UiTree) -> proc_macro2::TokenStream {
    expand_ui_node(&tree.root)
}

fn expand_ui_node(node: &UiNode) -> proc_macro2::TokenStream {
    let component = &node.component;
    let props = expand_props(&node.props);
    let children = expand_children(&node.children);
    
    quote! {
        {
            let mut component = #component::new();
            #props
            #children
            component
        }
    }
}

fn expand_props(props: &[UiProp]) -> proc_macro2::TokenStream {
    let prop_setters = props.iter().map(|prop| {
        let name = &prop.name;
        let value = &prop.value;
        quote! {
            component.#name(#value);
        }
    });
    
    quote! {
        #(#prop_setters)*
    }
}

fn expand_children(children: &[UiChild]) -> proc_macro2::TokenStream {
    let child_exprs = children.iter().map(|child| {
        match child {
            UiChild::Node(node) => {
                let child_expr = expand_ui_node(node);
                quote! {
                    component.add_child(#child_expr);
                }
            }
            UiChild::Text(text) => {
                quote! {
                    component.add_child(Text::new(#text));
                }
            }
            UiChild::Expression(expr) => {
                quote! {
                    component.add_child(#expr);
                }
            }
            UiChild::Conditional { condition, body } => {
                let body_expr = expand_ui_node(body);
                quote! {
                    if #condition {
                        component.add_child(#body_expr);
                    }
                }
            }
            UiChild::Loop { var, iter, body } => {
                let body_expr = expand_ui_node(body);
                quote! {
                    for #var in #iter {
                        component.add_child(#body_expr);
                    }
                }
            }
        }
    });
    
    quote! {
        #(#child_exprs)*
    }
}

fn expand_reactive_vars(vars: &ReactiveVars) -> proc_macro2::TokenStream {
    let var_defs = vars.vars.iter().map(|var| {
        let name = &var.name;
        let ty = &var.ty;
        let init = &var.init;
        let getter = quote::format_ident!("get_{}", name);
        let setter = quote::format_ident!("set_{}", name);
        
        quote! {
            let #name = ::std::cell::RefCell::new(#init);
            
            let #getter = || -> #ty {
                #name.borrow().clone()
            };
            
            let #setter = |value: #ty| {
                *#name.borrow_mut() = value;
                // Trigger reactive updates
                ::fluentai_renderer::reactive::trigger_update(stringify!(#name));
            };
        }
    });
    
    quote! {
        #(#var_defs)*
    }
}

fn expand_computed_props(props: &ComputedProps) -> proc_macro2::TokenStream {
    let prop_defs = props.props.iter().map(|prop| {
        let name = &prop.name;
        let expr = &prop.expr;
        let getter = quote::format_ident!("get_{}", name);
        
        quote! {
            let #getter = || {
                // Mark dependencies for tracking
                ::fluentai_renderer::reactive::with_tracking(|| {
                    #expr
                })
            };
        }
    });
    
    quote! {
        #(#prop_defs)*
    }
}

fn expand_effect(effect: &EffectDef) -> proc_macro2::TokenStream {
    let deps = &effect.deps;
    let body = &effect.body;
    
    let dep_names: Vec<_> = deps.iter().map(|d| d.to_string()).collect();
    
    quote! {
        ::fluentai_renderer::reactive::create_effect(
            vec![#(#dep_names),*],
            Box::new(move || #body)
        );
    }
}