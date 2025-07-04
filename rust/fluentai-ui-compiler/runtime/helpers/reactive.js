// Helper functions for reactive state management
const reactive = {
  ref(initialValue) {
    let value = initialValue;
    const listeners = new Set();
    
    return {
      get value() {
        // Track dependency if in reactive context
        if (reactive._currentWatcher) {
          listeners.add(reactive._currentWatcher);
        }
        return value;
      },
      
      set value(newValue) {
        if (value !== newValue) {
          value = newValue;
          // Notify all listeners
          listeners.forEach(listener => listener());
        }
      },
      
      _addListener(listener) {
        listeners.add(listener);
        return () => listeners.delete(listener);
      }
    };
  },
  
  computed(fn) {
    const result = reactive.ref();
    
    const update = () => {
      const prevWatcher = reactive._currentWatcher;
      reactive._currentWatcher = update;
      try {
        result.value = fn();
      } finally {
        reactive._currentWatcher = prevWatcher;
      }
    };
    
    update();
    return result;
  },
  
  watch(source, callback) {
    let oldValue = source.value;
    
    const unsubscribe = source._addListener(() => {
      const newValue = source.value;
      callback(newValue, oldValue);
      oldValue = newValue;
    });
    
    return unsubscribe;
  },
  
  _currentWatcher: null
};