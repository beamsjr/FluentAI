//! Random effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use claudelang_core::{ast::EffectType, value::Value, error::Error};
use rand::{thread_rng, Rng};

pub struct RandomHandler;

impl RandomHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl EffectHandler for RandomHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Random
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        let mut rng = thread_rng();
        
        match operation {
            "random" => {
                Ok(Value::Float(rng.gen::<f64>()))
            }
            "int" => {
                if args.len() >= 2 {
                    if let (Some(Value::Integer(min)), Some(Value::Integer(max))) = 
                        (args.get(0), args.get(1)) {
                        Ok(Value::Integer(rng.gen_range(*min..=*max)))
                    } else {
                        Err(Error::Runtime("random:int requires integer bounds".to_string()))
                    }
                } else {
                    Ok(Value::Integer(rng.gen()))
                }
            }
            "float" => {
                if args.len() >= 2 {
                    if let (Some(Value::Float(min)), Some(Value::Float(max))) = 
                        (args.get(0), args.get(1)) {
                        Ok(Value::Float(rng.gen_range(*min..=*max)))
                    } else {
                        Err(Error::Runtime("random:float requires float bounds".to_string()))
                    }
                } else {
                    Ok(Value::Float(rng.gen()))
                }
            }
            "bool" => {
                Ok(Value::Boolean(rng.gen()))
            }
            "choice" => {
                if let Some(Value::List(items)) = args.first() {
                    if items.is_empty() {
                        Err(Error::Runtime("random:choice requires non-empty list".to_string()))
                    } else {
                        let idx = rng.gen_range(0..items.len());
                        Ok(items[idx].clone())
                    }
                } else {
                    Err(Error::Runtime("random:choice requires a list".to_string()))
                }
            }
            "shuffle" => {
                if let Some(Value::List(items)) = args.first() {
                    let mut shuffled = items.clone();
                    use rand::seq::SliceRandom;
                    shuffled.shuffle(&mut rng);
                    Ok(Value::List(shuffled))
                } else {
                    Err(Error::Runtime("random:shuffle requires a list".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!("Unknown Random operation: {}", operation))),
        }
    }
}