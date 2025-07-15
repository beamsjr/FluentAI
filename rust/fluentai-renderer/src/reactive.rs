//! Reactive state management for Continuum UI
//! 
//! This module implements the reactive engine that automatically updates UI
//! when state fields change, fulfilling Phase 3's reactive requirements.

use std::collections::{HashMap, HashSet};
use std::cell::RefCell;
use std::rc::Rc;
use serde_json::Value as JsonValue;

/// A reactive state field that tracks dependencies and notifies observers
#[derive(Clone)]
pub struct StateField {
    name: String,
    value: RefCell<JsonValue>,
    observers: RefCell<Vec<StateObserver>>,
}

impl StateField {
    /// Create a new state field
    pub fn new(name: String, initial_value: JsonValue) -> Self {
        Self {
            name,
            value: RefCell::new(initial_value),
            observers: RefCell::new(Vec::new()),
        }
    }
    
    /// Get the current value
    pub fn get(&self) -> JsonValue {
        // Track this access if we're in a reactive context
        if let Some(tracker) = DEPENDENCY_TRACKER.with(|t| t.borrow().clone()) {
            tracker.track_dependency(self.name.clone());
        }
        self.value.borrow().clone()
    }
    
    /// Set a new value and trigger reactive updates
    pub fn set(&self, new_value: JsonValue) {
        let changed = {
            let mut value = self.value.borrow_mut();
            if *value != new_value {
                *value = new_value;
                true
            } else {
                false
            }
        };
        
        if changed {
            self.notify_observers();
        }
    }
    
    /// Disturb the field (increment for numbers, toggle for booleans, etc.)
    pub fn disturb(&self) {
        match &*self.value.borrow() {
            JsonValue::Number(n) => {
                if let Some(num) = n.as_i64() {
                    self.set(JsonValue::Number((num + 1).into()));
                }
            }
            JsonValue::Bool(b) => {
                self.set(JsonValue::Bool(!b));
            }
            _ => {} // Other types don't have a default disturb behavior
        }
    }
    
    /// Add an observer that will be called when this field changes
    pub fn observe(&self, observer: StateObserver) {
        self.observers.borrow_mut().push(observer);
    }
    
    /// Notify all observers of a change
    fn notify_observers(&self) {
        let observers = self.observers.borrow();
        for observer in observers.iter() {
            observer.on_change(&self.name);
        }
    }
}

/// Observer that reacts to state changes
pub trait Observer {
    fn on_change(&self, field_name: &str);
}

impl<F: Fn(&str)> Observer for F {
    fn on_change(&self, field_name: &str) {
        self(field_name)
    }
}

pub type StateObserver = Rc<dyn Observer>;

/// Tracks dependencies during reactive computations
#[derive(Clone)]
struct DependencyTracker {
    current_computation: Rc<RefCell<HashSet<String>>>,
}

impl DependencyTracker {
    fn new() -> Self {
        Self {
            current_computation: Rc::new(RefCell::new(HashSet::new())),
        }
    }
    
    fn track_dependency(&self, field_name: String) {
        self.current_computation.borrow_mut().insert(field_name);
    }
    
    fn get_dependencies(&self) -> HashSet<String> {
        self.current_computation.borrow().clone()
    }
    
    fn clear(&self) {
        self.current_computation.borrow_mut().clear();
    }
}

thread_local! {
    static DEPENDENCY_TRACKER: RefCell<Option<DependencyTracker>> = RefCell::new(None);
}

/// Reactive computation that automatically re-runs when dependencies change
pub struct ReactiveComputation {
    computation: Box<dyn Fn()>,
    dependencies: RefCell<HashSet<String>>,
}

impl ReactiveComputation {
    /// Create a new reactive computation
    pub fn new<F: Fn() + 'static>(computation: F) -> Rc<Self> {
        let comp = Rc::new(Self {
            computation: Box::new(computation),
            dependencies: RefCell::new(HashSet::new()),
        });
        
        // Run once to establish dependencies
        comp.run();
        
        comp
    }
    
    /// Run the computation and track dependencies
    pub fn run(&self) {
        // Set up dependency tracking
        let tracker = DependencyTracker::new();
        DEPENDENCY_TRACKER.with(|t| *t.borrow_mut() = Some(tracker.clone()));
        
        // Run the computation
        (self.computation)();
        
        // Get tracked dependencies
        let new_deps = tracker.get_dependencies();
        
        // Clear global tracker
        DEPENDENCY_TRACKER.with(|t| *t.borrow_mut() = None);
        
        // Update our dependencies
        *self.dependencies.borrow_mut() = new_deps;
    }
}

/// Manages all reactive state for a Continuum UI application
pub struct ReactiveEngine {
    state_fields: HashMap<String, Rc<StateField>>,
    computations: Vec<Rc<ReactiveComputation>>,
    event_handlers: HashMap<String, Vec<Box<dyn Fn(JsonValue)>>>,
}

impl ReactiveEngine {
    /// Create a new reactive engine
    pub fn new() -> Self {
        Self {
            state_fields: HashMap::new(),
            computations: Vec::new(),
            event_handlers: HashMap::new(),
        }
    }
    
    /// Register a state field
    pub fn register_state_field(&mut self, name: String, field_type: Option<String>, initial: Option<JsonValue>) {
        let initial_value = initial.unwrap_or_else(|| {
            match field_type.as_deref() {
                Some("int") => JsonValue::Number(0.into()),
                Some("float") => JsonValue::Number(serde_json::Number::from_f64(0.0).unwrap()),
                Some("bool") => JsonValue::Bool(false),
                Some("string") => JsonValue::String(String::new()),
                _ => JsonValue::Null,
            }
        });
        
        let field = Rc::new(StateField::new(name.clone(), initial_value));
        
        // Create a weak reference to self to avoid circular references
        let weak_self = Rc::downgrade(&Rc::new(self as *const ReactiveEngine));
        let field_name_clone = name.clone();
        
        // Set up observer that will look up computations dynamically
        let observer = Rc::new(move |changed_field: &str| {
            // This closure captures the field name and will check computations at runtime
            // For now, we'll use a different approach
            let _ = (weak_self.clone(), field_name_clone.clone(), changed_field);
        });
        
        field.observe(observer);
        
        self.state_fields.insert(name.clone(), field.clone());
        
        // Update the field's observer whenever computations change
        self.update_field_observer(&name, field);
    }
    
    /// Update a field's observer to include all current computations
    fn update_field_observer(&self, field_name: &str, field: Rc<StateField>) {
        // Clear existing observers
        field.observers.borrow_mut().clear();
        
        // Create new observer with current computations
        let computations = self.computations.clone();
        let field_name_clone = field_name.to_string();
        
        let observer: StateObserver = Rc::new(move |_: &str| {
            for comp in &computations {
                if comp.dependencies.borrow().contains(&field_name_clone) {
                    comp.run();
                }
            }
        });
        
        field.observe(observer);
    }
    
    /// Get a state field by name
    pub fn get_field(&self, name: &str) -> Option<Rc<StateField>> {
        self.state_fields.get(name).cloned()
    }
    
    /// Register a reactive computation
    pub fn add_computation<F: Fn() + 'static>(&mut self, computation: F) {
        let comp = ReactiveComputation::new(computation);
        self.computations.push(comp);
        
        // Update all field observers to include this new computation
        for (field_name, field) in &self.state_fields {
            self.update_field_observer(field_name, field.clone());
        }
    }
    
    /// Handle a UI event
    pub fn handle_event(&self, element_id: &str, event_type: &str, event_data: JsonValue) {
        let key = format!("{}:{}", element_id, event_type);
        if let Some(handlers) = self.event_handlers.get(&key) {
            for handler in handlers {
                handler(event_data.clone());
            }
        }
    }
    
    /// Register an event handler
    pub fn register_event_handler<F: Fn(JsonValue) + 'static>(
        &mut self,
        element_id: String,
        event_type: String,
        handler: F,
    ) {
        let key = format!("{}:{}", element_id, event_type);
        self.event_handlers
            .entry(key)
            .or_insert_with(Vec::new)
            .push(Box::new(handler));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_reactive_state() {
        let mut engine = ReactiveEngine::new();
        
        // Register a counter state field
        engine.register_state_field("counter".to_string(), Some("int".to_string()), None);
        
        // Track how many times our computation runs
        let run_count = Rc::new(RefCell::new(0));
        let run_count_clone = run_count.clone();
        
        // Store weak reference to engine fields for the computation
        let counter_field = engine.get_field("counter").unwrap();
        let counter_field_clone = counter_field.clone();
        
        // Add a reactive computation that depends on the counter
        engine.add_computation(move || {
            *run_count_clone.borrow_mut() += 1;
            
            // Access the counter value to establish dependency
            let _value = counter_field_clone.get();
            
            // Track dependency manually for now
            DEPENDENCY_TRACKER.with(|t| {
                if let Some(ref tracker) = *t.borrow() {
                    tracker.track_dependency("counter".to_string());
                }
            });
        });
        
        // Initial run
        assert_eq!(*run_count.borrow(), 1);
        
        // Change the counter
        if let Some(counter) = engine.get_field("counter") {
            counter.set(JsonValue::Number(5.into()));
        }
        
        // Computation should have run again
        assert_eq!(*run_count.borrow(), 2);
    }
}