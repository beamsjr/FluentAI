//! Random effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, value::Value, error::Error};
use rand::{thread_rng, Rng, SeedableRng};
use rand::rngs::StdRng;
use std::sync::{Arc, Mutex};

pub struct RandomHandler {
    rng: Arc<Mutex<StdRng>>,
}

impl RandomHandler {
    pub fn new() -> Self {
        Self {
            rng: Arc::new(Mutex::new(StdRng::from_entropy())),
        }
    }
}

#[async_trait]
impl EffectHandler for RandomHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Random
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "seed" => {
                if let Some(Value::Integer(seed)) = args.first() {
                    let mut rng = self.rng.lock().unwrap();
                    *rng = StdRng::seed_from_u64(*seed as u64);
                    Ok(Value::Nil)
                } else {
                    Err(Error::Runtime("random:seed requires integer seed".to_string()))
                }
            }
            "random" => {
                let mut rng = self.rng.lock().unwrap();
                Ok(Value::Float(rng.gen::<f64>()))
            }
            "int" => {
                let mut rng = self.rng.lock().unwrap();
                if args.len() >= 2 {
                    if let (Some(Value::Integer(min)), Some(Value::Integer(max))) = 
                        (args.get(0), args.get(1)) {
                        Ok(Value::Integer(rng.gen_range(*min..*max)))
                    } else {
                        Err(Error::Runtime("random:int requires integer bounds".to_string()))
                    }
                } else {
                    Ok(Value::Integer(rng.gen()))
                }
            }
            "float" => {
                let mut rng = self.rng.lock().unwrap();
                if args.len() >= 2 {
                    if let (Some(Value::Float(min)), Some(Value::Float(max))) = 
                        (args.get(0), args.get(1)) {
                        Ok(Value::Float(rng.gen_range(*min..*max)))
                    } else {
                        Err(Error::Runtime("random:float requires float bounds".to_string()))
                    }
                } else {
                    Ok(Value::Float(rng.gen()))
                }
            }
            "bool" => {
                let mut rng = self.rng.lock().unwrap();
                Ok(Value::Boolean(rng.gen()))
            }
            "choice" => {
                if let Some(Value::List(items)) = args.first() {
                    if items.is_empty() {
                        Err(Error::Runtime("random:choice requires non-empty list".to_string()))
                    } else {
                        let mut rng = self.rng.lock().unwrap();
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
                    let mut rng = self.rng.lock().unwrap();
                    shuffled.shuffle(&mut *rng);
                    Ok(Value::List(shuffled))
                } else {
                    Err(Error::Runtime("random:shuffle requires a list".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!("Unknown Random operation: {}", operation))),
        }
    }
}