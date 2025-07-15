// FluentAI Runtime using WASM compiler
// This replaces the mock runtime with the real FluentAI compiler

import init, { FluentAI as WasmFluentAI } from '../www/pkg/fluentai_renderer.js';

export class FluentAI {
    constructor() {
        this.wasmModule = null;
        this.initialized = false;
        this.fluentai = null;
    }

    static async initialize(config) {
        const runtime = new FluentAI();
        
        try {
            // Initialize WASM module
            console.log('Initializing FluentAI WASM module...');
            runtime.wasmModule = await init();
            
            // Create FluentAI instance
            runtime.fluentai = new WasmFluentAI();
            runtime.initialized = true;
            
            // Set optimization level if specified
            if (config.optimization) {
                runtime.fluentai.set_optimization_level(config.optimization);
            }
            
            // Enable debug info if specified
            if (config.debug !== undefined) {
                runtime.fluentai.set_debug_info(config.debug);
            }
            
            console.log('FluentAI WASM module initialized successfully');
            
        } catch (error) {
            console.error('Failed to initialize FluentAI WASM:', error);
            throw error;
        }
        
        return runtime;
    }

    async compile(source) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Compile returns Uint8Array of bytecode
            return this.fluentai.compile(source);
        } catch (error) {
            console.error('Compilation error:', error);
            throw error;
        }
    }
    
    async compileWithMetadata(source) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Returns metadata about the compilation
            return this.fluentai.compile_with_metadata(source);
        } catch (error) {
            console.error('Compilation error:', error);
            throw error;
        }
    }

    async parse(source) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Parse and return AST info
            return this.fluentai.parse(source);
        } catch (error) {
            console.error('Parse error:', error);
            throw error;
        }
    }

    async run(source) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Compile and execute
            return this.fluentai.run(source);
        } catch (error) {
            console.error('Runtime error:', error);
            throw error;
        }
    }

    async execute(bytecode) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Execute pre-compiled bytecode
            return this.fluentai.execute(bytecode);
        } catch (error) {
            console.error('Execution error:', error);
            throw error;
        }
    }

    async callFunction(name, ...args) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        try {
            // Call a specific function
            return this.fluentai.call_function(name, args);
        } catch (error) {
            console.error('Function call error:', error);
            throw error;
        }
    }

    getGlobal(name) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        return this.fluentai.get_global(name);
    }

    setGlobal(name, value) {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        this.fluentai.set_global(name, value);
    }

    getStats() {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        return this.fluentai.get_stats();
    }

    // Run a simple demo to test the system
    async runDemo() {
        if (!this.initialized) {
            throw new Error('FluentAI not initialized. Call FluentAI.initialize() first.');
        }
        
        return this.fluentai.run_demo();
    }
}

// Re-export any other WASM exports that might be useful
export { init };