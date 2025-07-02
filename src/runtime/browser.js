/**
 * ClaudeLang Browser Runtime
 * 
 * This provides the runtime support for ClaudeLang programs compiled to JavaScript
 * and running in the browser.
 */

(function(global) {
    'use strict';

    // Core runtime object
    const ClaudeLang = {
        version: '0.1.0',
        effects: {},
        primitives: {},
        modules: {},
        _effectHandlers: new Map(),
        _currentEffect: null
    };

    // Virtual DOM implementation
    class VNode {
        constructor(tag, props, children) {
            this.tag = tag;
            this.props = props || {};
            this.children = children || [];
            this.key = props?.key;
        }

        toElement() {
            if (this.tag === 'text') {
                return document.createTextNode(this.props.content || '');
            }

            if (this.tag === 'fragment') {
                const fragment = document.createDocumentFragment();
                this.children.forEach(child => {
                    fragment.appendChild(child.toElement());
                });
                return fragment;
            }

            const el = document.createElement(this.tag);

            // Set properties
            Object.entries(this.props).forEach(([key, value]) => {
                if (key.startsWith('on')) {
                    const event = key.slice(2).toLowerCase();
                    el.addEventListener(event, value);
                } else if (key === 'style') {
                    if (typeof value === 'object') {
                        Object.assign(el.style, value);
                    } else {
                        el.style.cssText = value;
                    }
                } else if (key === 'class' || key === 'className') {
                    el.className = value;
                } else if (key === 'checked' || key === 'selected' || key === 'disabled') {
                    el[key] = Boolean(value);
                } else if (key === 'value') {
                    el.value = value;
                } else if (key !== 'key' && value != null) {
                    el.setAttribute(key, value);
                }
            });

            // Add children
            this.children.forEach(child => {
                if (typeof child === 'string') {
                    el.appendChild(document.createTextNode(child));
                } else if (child instanceof VNode) {
                    el.appendChild(child.toElement());
                }
            });

            return el;
        }
    }

    // DOM primitives
    ClaudeLang.primitives['dom:h'] = function(tag, props, children) {
        return new VNode(tag, props, children);
    };

    ClaudeLang.primitives['dom:text'] = function(content) {
        return new VNode('text', { content }, []);
    };

    ClaudeLang.primitives['dom:fragment'] = function(children) {
        return new VNode('fragment', {}, children);
    };

    // Reactive state implementation
    class ReactiveRef {
        constructor(initialValue) {
            this._value = initialValue;
            this._watchers = new Set();
            this._id = 'ref_' + Math.random().toString(36).substr(2, 9);
        }

        get value() {
            // Track dependency if we're in a reactive context
            if (ClaudeLang._trackingContext) {
                ClaudeLang._trackingContext.track(this._id);
            }
            return this._value;
        }

        set value(newValue) {
            if (this._value !== newValue) {
                this._value = newValue;
                this._notify();
            }
        }

        watch(callback) {
            this._watchers.add(callback);
            return () => this._watchers.delete(callback);
        }

        _notify() {
            this._watchers.forEach(callback => callback(this._value));
        }
    }

    class ComputedRef {
        constructor(computeFn) {
            this._computeFn = computeFn;
            this._value = undefined;
            this._dirty = true;
            this._deps = new Set();
            this._watchers = new Set();
        }

        get value() {
            if (this._dirty) {
                this._recompute();
            }
            return this._value;
        }

        _recompute() {
            // Clear old dependencies
            this._deps.forEach(dep => {
                // Unsubscribe from old deps
            });
            this._deps.clear();

            // Track new dependencies
            const prevContext = ClaudeLang._trackingContext;
            ClaudeLang._trackingContext = this;

            try {
                this._value = this._computeFn();
                this._dirty = false;
            } finally {
                ClaudeLang._trackingContext = prevContext;
            }
        }

        track(depId) {
            this._deps.add(depId);
        }

        invalidate() {
            this._dirty = true;
            this._notify();
        }

        _notify() {
            this._watchers.forEach(callback => callback(this.value));
        }
    }

    // Reactive primitives
    ClaudeLang.primitives['ui:ref'] = function(initialValue) {
        return new ReactiveRef(initialValue);
    };

    ClaudeLang.primitives['ui:computed'] = function(computeFn) {
        return new ComputedRef(computeFn);
    };

    ClaudeLang.primitives['ui:watch'] = function(deps, callback, options) {
        // Simplified watch implementation
        if (Array.isArray(deps)) {
            deps.forEach(dep => {
                if (dep instanceof ReactiveRef) {
                    dep.watch(() => callback());
                }
            });
        }
    };

    // Effect handlers
    ClaudeLang.performEffect = function(effectType, operation, args) {
        const handler = this._effectHandlers.get(effectType);
        if (!handler) {
            throw new Error(`No handler for effect type: ${effectType}`);
        }
        return handler(operation, args);
    };

    // DOM effect handler
    ClaudeLang._effectHandlers.set('DOM', function(operation, args) {
        switch (operation) {
            case 'render':
                const [vnode, container] = args;
                const root = typeof container === 'string' 
                    ? document.querySelector(container) 
                    : container;
                
                if (!root) {
                    throw new Error(`Container not found: ${container}`);
                }

                // Simple render - replace content
                root.innerHTML = '';
                if (vnode instanceof VNode) {
                    root.appendChild(vnode.toElement());
                }
                break;

            case 'create-element':
                const [tag, props, children] = args;
                return new VNode(tag, props, children);

            case 'query-selector':
                return document.querySelector(args[0]);

            default:
                throw new Error(`Unknown DOM operation: ${operation}`);
        }
    });

    // Reactive effect handler
    ClaudeLang._effectHandlers.set('STATE', function(operation, args) {
        switch (operation) {
            case 'reactive:ref':
                return new ReactiveRef(args[0]);

            case 'reactive:get':
                const ref = args[0];
                return ref instanceof ReactiveRef ? ref.value : ref;

            case 'reactive:set':
                const [refToSet, value] = args;
                if (refToSet instanceof ReactiveRef) {
                    refToSet.value = value;
                }
                break;

            case 'reactive:update':
                const [refToUpdate, updater] = args;
                if (refToUpdate instanceof ReactiveRef) {
                    refToUpdate.value = updater(refToUpdate.value);
                }
                break;

            case 'reactive:computed':
                return new ComputedRef(args[0]);

            default:
                throw new Error(`Unknown reactive operation: ${operation}`);
        }
    });

    // Time effect handler
    ClaudeLang._effectHandlers.set('TIME', function(operation, args) {
        switch (operation) {
            case 'now':
                return Date.now();

            case 'sleep':
                return new Promise(resolve => setTimeout(resolve, args[0]));

            default:
                throw new Error(`Unknown time operation: ${operation}`);
        }
    });

    // IO effect handler (console in browser)
    ClaudeLang._effectHandlers.set('IO', function(operation, args) {
        switch (operation) {
            case 'print':
                console.log(...args);
                break;

            case 'print-err':
                console.error(...args);
                break;

            default:
                throw new Error(`Unknown IO operation: ${operation}`);
        }
    });

    // Standard library functions
    ClaudeLang.primitives['+'] = (a, b) => a + b;
    ClaudeLang.primitives['-'] = (a, b) => a - b;
    ClaudeLang.primitives['*'] = (a, b) => a * b;
    ClaudeLang.primitives['/'] = (a, b) => a / b;
    ClaudeLang.primitives['='] = (a, b) => a === b;
    ClaudeLang.primitives['<'] = (a, b) => a < b;
    ClaudeLang.primitives['>'] = (a, b) => a > b;
    ClaudeLang.primitives['<='] = (a, b) => a <= b;
    ClaudeLang.primitives['>='] = (a, b) => a >= b;
    ClaudeLang.primitives['not'] = (a) => !a;
    ClaudeLang.primitives['and'] = (a, b) => a && b;
    ClaudeLang.primitives['or'] = (a, b) => a || b;

    // List operations
    ClaudeLang.primitives['cons'] = (head, tail) => [head, ...tail];
    ClaudeLang.primitives['car'] = (list) => list[0];
    ClaudeLang.primitives['cdr'] = (list) => list.slice(1);
    ClaudeLang.primitives['empty?'] = (list) => list.length === 0;
    ClaudeLang.primitives['length'] = (list) => list.length;
    ClaudeLang.primitives['map'] = (fn, list) => list.map(fn);
    ClaudeLang.primitives['filter'] = (fn, list) => list.filter(fn);
    ClaudeLang.primitives['reduce'] = (fn, init, list) => list.reduce(fn, init);

    // String operations
    ClaudeLang.primitives['concat'] = (...args) => args.join('');
    ClaudeLang.primitives['to-string'] = (val) => String(val);

    // Utility functions
    ClaudeLang.primitives['get'] = (obj, key) => obj?.[key];
    ClaudeLang.primitives['set'] = (obj, key, val) => ({ ...obj, [key]: val });

    // Conditional helpers
    ClaudeLang.primitives['ui:if'] = (cond, thenVal, elseVal) => cond ? thenVal : elseVal;
    ClaudeLang.primitives['ui:when'] = (cond, val) => cond ? val : null;
    ClaudeLang.primitives['ui:for'] = (list, fn) => list.map(fn);

    // Style helpers
    ClaudeLang.primitives['ui:style'] = (styleObj) => {
        if (typeof styleObj === 'object') {
            return Object.entries(styleObj)
                .map(([key, value]) => {
                    // Convert camelCase to kebab-case
                    const cssKey = key.replace(/([A-Z])/g, '-$1').toLowerCase();
                    return `${cssKey}: ${value}`;
                })
                .join('; ');
        }
        return styleObj;
    };

    ClaudeLang.primitives['ui:class'] = (...args) => {
        const classes = [];
        args.forEach(arg => {
            if (typeof arg === 'string') {
                classes.push(arg);
            } else if (Array.isArray(arg)) {
                classes.push(...arg);
            } else if (typeof arg === 'object') {
                Object.entries(arg).forEach(([cls, condition]) => {
                    if (condition) classes.push(cls);
                });
            }
        });
        return classes.filter(Boolean).join(' ');
    };

    // Module system
    ClaudeLang.defineModule = function(name, exports) {
        this.modules[name] = exports;
    };

    ClaudeLang.require = function(name) {
        if (!this.modules[name]) {
            throw new Error(`Module not found: ${name}`);
        }
        return this.modules[name];
    };

    // Component system
    ClaudeLang.components = {};
    
    ClaudeLang.defineComponent = function(name, definition) {
        this.components[name] = definition;
        
        // Create a component constructor
        const ComponentConstructor = function(props) {
            // Validate props
            const validatedProps = ClaudeLang.validateProps(definition.props, props);
            
            // Call render function
            return definition.render(validatedProps);
        };
        
        // Store the constructor too
        this.components[name].Constructor = ComponentConstructor;
        
        return ComponentConstructor;
    };
    
    ClaudeLang.validateProps = function(propDefs, props) {
        const validated = {};
        
        for (const [name, def] of Object.entries(propDefs)) {
            if (props.hasOwnProperty(name)) {
                validated[name] = props[name];
            } else if (def.required) {
                throw new Error(`Required prop '${name}' is missing`);
            } else if (def.default !== null && def.default !== undefined) {
                validated[name] = def.default;
            }
        }
        
        return validated;
    };
    
    // Component instantiation primitive
    ClaudeLang.primitives['ui:create'] = function(component, props) {
        if (typeof component === 'string') {
            // Look up component by name
            const ComponentDef = ClaudeLang.components[component];
            if (!ComponentDef) {
                throw new Error(`Component '${component}' not found`);
            }
            return ComponentDef.Constructor(props);
        } else if (typeof component === 'function') {
            // Direct component function
            return component(props);
        } else if (component && component.Constructor) {
            // Component definition object
            return component.Constructor(props);
        } else {
            throw new Error('Invalid component');
        }
    };

    // UI Optimization Support
    ClaudeLang.optimizations = {
        components: {}
    };

    ClaudeLang.renderMetrics = {};

    // Configure optimizations for components
    ClaudeLang.configureOptimizations = function(config) {
        if (config.components) {
            Object.assign(this.optimizations.components, config.components);
        }
    };

    // Memoization helper
    ClaudeLang.memoize = function(component, propKeys) {
        const cache = new Map();
        
        return function(props) {
            // Create cache key from specified props
            const key = propKeys.map(k => props[k]).join('|');
            
            if (cache.has(key)) {
                return cache.get(key);
            }
            
            const result = component(props);
            cache.set(key, result);
            
            // Limit cache size
            if (cache.size > 100) {
                const firstKey = cache.keys().next().value;
                cache.delete(firstKey);
            }
            
            return result;
        };
    };

    // Lazy loading helper
    ClaudeLang.lazy = function(loader) {
        let Component = null;
        let promise = null;
        
        return function LazyComponent(props) {
            if (Component) {
                return Component(props);
            }
            
            if (!promise) {
                promise = loader().then(c => {
                    Component = c;
                    return c;
                });
            }
            
            // Return loading placeholder
            return ClaudeLang.primitives['dom:h']('div', {}, [
                ClaudeLang.primitives['dom:text']('Loading...')
            ]);
        };
    };

    // Batch updates configuration
    ClaudeLang.batchingConfig = {};

    ClaudeLang.configureBatching = function(componentName, config) {
        this.batchingConfig[componentName] = config;
    };

    // Enhanced render tracking
    const originalRender = ClaudeLang.performEffect;
    ClaudeLang.performEffect = function(effectType, operation, args) {
        if (effectType === 'DOM' && operation === 'render') {
            const startTime = performance.now();
            const [vnode, container] = args;
            
            // Apply render optimizations if configured
            if (vnode && vnode.type && ClaudeLang.optimizations.components[vnode.type]) {
                const opts = ClaudeLang.optimizations.components[vnode.type];
                
                // Apply throttling
                if (opts.throttle) {
                    const lastRender = ClaudeLang.renderMetrics[vnode.type]?.lastRender || 0;
                    const timeSince = startTime - lastRender;
                    
                    if (timeSince < opts.throttle) {
                        return; // Skip render
                    }
                }
            }
            
            const result = originalRender.call(this, effectType, operation, args);
            
            const endTime = performance.now();
            const renderTime = endTime - startTime;
            
            // Track render metrics
            if (vnode && vnode.type) {
                if (!ClaudeLang.renderMetrics[vnode.type]) {
                    ClaudeLang.renderMetrics[vnode.type] = {
                        renders: [],
                        lastRender: 0
                    };
                }
                
                ClaudeLang.renderMetrics[vnode.type].renders.push({
                    time: renderTime,
                    timestamp: startTime,
                    propsChanged: vnode.propsChanged || []
                });
                
                ClaudeLang.renderMetrics[vnode.type].lastRender = startTime;
                
                // Keep only last 100 renders
                if (ClaudeLang.renderMetrics[vnode.type].renders.length > 100) {
                    ClaudeLang.renderMetrics[vnode.type].renders.shift();
                }
            }
            
            return result;
        } else {
            return originalRender.call(this, effectType, operation, args);
        }
    };

    // Export render metrics for analysis
    ClaudeLang.getRenderMetrics = function() {
        return this.renderMetrics;
    };

    // Export to global scope
    global.ClaudeLang = ClaudeLang;

    // Shortcuts for common operations
    global.h = ClaudeLang.primitives['dom:h'];
    global.ref = ClaudeLang.primitives['ui:ref'];
    global.computed = ClaudeLang.primitives['ui:computed'];
    global.render = (vnode, container) => 
        ClaudeLang.performEffect('DOM', 'render', [vnode, container]);

})(typeof window !== 'undefined' ? window : global);