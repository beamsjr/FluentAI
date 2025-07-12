  1. Always run the full workspace test suite after making any changes, no matter how isolated they seem
  2. Before declaring any fix complete, verify that all tests across the entire codebase still pass
  3. If a change breaks tests in other modules, investigate why and either:
    - Fix the broken tests if they were relying on incorrect behavior
    - Reconsider the change if it breaks legitimate functionality
    - Find a solution that satisfies all parts of the system
  4. Always make sure to follow the Language definition defined below. 


 This is especially important in a complex system like FluentAI where modules are interconnected in subtle ways.
 The LoadGlobal change was a perfect example - what seemed like a simple fix in the VM module ended up  breaking functionality in the effects module.

  Please make sure to follow this workflow for all future changes:
  1. Make change
  2. Run local module tests
  3. Run full workspace tests
  4. Only proceed if ALL tests pass
  5. If any test fails, investigate and resolve before moving on

Also very important for development, if you change something in critical parts of the system that heavely depends on order, please leave a comment on what test you changed somthing for, this way If we find later that this change broke something else we know why we made the change so we can make sure to get both issues resolved. 

## Common Pitfalls to Watch For

### Catch-all Pattern in Match Statements
When adding new cases to match statements (especially in optimizers or analyzers), be aware of catch-all patterns like `_ => {}`. These can silently ignore new node types if they're placed before your new case. For example:

```rust
match node {
    Node::Application { .. } => { /* handle */ }
    Node::Lambda { .. } => { /* handle */ }
    _ => {} // This catches everything else!
    Node::Channel { .. } => { /* This will NEVER be reached! */ }
}
```

Always check if there's a catch-all pattern and ensure your new cases are added BEFORE it. This is particularly important in:
- Dead code elimination (mark_reachable methods)
- Effect analysis 
- Node copying/transformation functions
- Any visitor pattern implementation

---

Refined FLC: The Complete Style Guide1. Core PhilosophyRefined FLC (Fluent Lambda Chain) is a general-purpose programming language designed for clarity, expressiveness, and safety. Its core philosophy is that all operations should, whenever possible, be expressed as a linear, left-to-right chain of transformations.This is achieved by making method chaining the primary mode of expression, enhanced by powerful, inline lambda expressions (param => ...) that allow for complex logic without breaking the fluent flow. The syntax prioritizes consistency, unambiguity, and human ergonomics, making it ideal for both developers and AI-driven tooling.2. Lexical StructureThis section defines the basic grammar and tokens of the language.Comments:// Single-line comment/* Multi-line comment */Identifiers & Naming Conventions:snake_case: For variables and function names (user_name, calculate_total).PascalCase: For all types (structs, enums, traits, actors) (User, RequestState).SCREAMING_SNAKE_CASE: For constants and enum variants (MAX_USERS, RequestState.Success).Keywords:Definitions: private, public, function, struct, enum, trait, as, type, actor, effect, macro, externControl Flow: if, else, match, case, for, while, in, let, constConcurrency: async, await, parallel, handleError Handling: try, catch, finallyModules: use, modOther: true, false, self, super, unsafe, dyn, with, performOperators:Chaining: . (method call), |> (pipe-to), .? (optional chaining)Lambda: =>Arithmetic: +, -, *, /, % (modulo)Comparison: ==, !=, <, >, <=, >=Logical: && (and), || (or), ! (not)Assignment: =, := (mutation)3. Primitive Types & LiteralsPrimitive Types:int: Integer (e.g., 10, -5).float: Floating-point number (e.g., 3.14).string: UTF-8 text (e.g., "hello").bool: Boolean (true or false).Collection Literals:List: [1, 2, 3]Map: {"key1": "value1", "key2": "value2"}Set: #{1, 2, 3}String Formatting: F-strings are used for easy interpolation.let name = "Alice";
let message = f"Hello, {name}!"; // "Hello, Alice!"
Console Output: The $(...) construct wraps a string in a "Printable" object. This is the idiomatic way to handle console I/O.$("This will be printed to the console.").print();

let name = "World";
$(f"Hello, {name}!").print(); // Works with formatted strings
4. Modules and Visibility

Modules provide encapsulation and organization. Each .flc file is implicitly a module.

Module Declaration:
// Optional module declaration at the top of a file
module ModuleName;

// If no module declaration is provided, the module name defaults to the filename
// File: Stock.flc → module Stock
// File: price_service.flc → module price_service

Automatic Exports:
- All `public` definitions are automatically exported from a module
- All `private` definitions remain module-internal
- No explicit `export` statements needed

Importing:
// Import a constructor or primary export
use Stock;
use Portfolio;

// Import specific public items
use Stock::{from_json, to_json};
use Portfolio::{calculate_value, rebalance};

// Import all public items from a module
use PriceService::*;

// Import with aliasing
use VeryLongModuleName as VLMN;
use SomeLongFunctionName as short_name from Utils;

// Relative imports
use ./utils/Helper;  // From relative path
use ../shared/Common;  // From parent directory

Module Path Resolution:
// Standard library modules use std:: prefix
use std::collections::List;
use std::io::File;

// Local modules use relative paths or module names
use ./models/User;  // Relative to current file
use Portfolio;      // Searches module path

Visibility:
private ...: Default visibility. The definition is private to the current module.
public ...: Public visibility. The definition is automatically exported and can be imported by other modules.

4.1 Object-Oriented Patterns

FluentAI supports object-oriented programming through closure patterns. This provides encapsulation and method chaining without requiring special class syntax.

Pattern Example:
// Stock.flc
module Stock;

public function Stock(symbol: string, shares: float, price: float) {
    // Private state - not accessible from outside
    let state = {
        "symbol": symbol,
        "shares": shares,
        "current_price": price
    };
    
    // Private helper functions
    let validate_shares = (shares) => shares >= 0.0;
    
    // Create the public interface
    let self = {
        "get_symbol": () => state.symbol,
        "get_shares": () => state.shares,
        "get_value": () => state.shares * state.current_price,
        
        "update_shares": (new_shares) => {
            if (validate_shares(new_shares)) {
                state.shares = new_shares;
            }
            self  // Return self for method chaining
        },
        
        "update_price": (new_price) => {
            if (new_price > 0.0) {
                state.current_price = new_price;
            }
            self
        },
        
        "to_json": () => {
            {"symbol": state.symbol, "shares": state.shares, "price": state.current_price}
        }
    };
    
    return self;
}

// Additional module-level functions
public function from_json(data) {
    Stock(data.symbol, data.shares, data.price)
}

Usage from Another Module:
// main.flc
use Stock;
use Stock::{from_json};

let my_stock = Stock("AAPL", 100, 150.0);
my_stock
    .update_price(155.0)
    .update_shares(110);

let value = my_stock.get_value();  // 17,050.0
$(f"Stock value: ${value}").print();

// Using the additional function
let stock_data = {"symbol": "GOOGL", "shares": 50, "price": 2800.0};
let google = from_json(stock_data);

5. Definitions & Declarations

All top-level definitions start with a visibility keyword (public or private).

Functions:
// Top-level function
public function create_user(name: string) -> User { ... }

// Private functions can be defined at top-level
private function helper() { ... }

// Note: Inside blocks/expressions, use let bindings for functions:
{
    let local_func = (x) => x * 2;
    local_func(5)
}

Structs (Value Types):
public struct User {
    id: Uuid, // Private field
    pub name: string, // Public field
}
Enums:
public enum RequestState { ... }
Traits (Interfaces):
public trait Serializable { ... }
Trait Implementations: The as keyword is used to implement a trait for a type.
private User as Serializable {
    private function to_json(self) -> string {
        // implementation...
    }
}
6. Chaining, Lambdas, and Control Flow

The core of FLC is the fluent chain.

Implicit Iteration: Collection processing methods like .map, .filter, and .for_each_async can be called directly on collections. There is no need for an explicit .iter() call.

// Correct:
[1, 2, 3].map(x => x * 2)

// Incorrect (unnecessary):
[1, 2, 3].iter().map(x => x * 2)
Lambda Expressions: Lambdas are passed as arguments to methods. The => operator separates parameters from the body.

// Single parameter
users.filter(user => user.age > 18)

// Multiple parameters
numbers.reduce(0, (acc, item) => acc + item)

// No parameters
on_done(() => $("Task complete!").print())
Destructuring in Lambdas:
users.map(({name, email}) => f"{name} <{email}>")
Control Flow as Chainable Expressions:
let message = if (user.is_active) { "Active" } else { "Inactive" };

// The `match` expression is a powerful tool in chains.
let status_text = response
    .match()
        // For simple value mapping
        .case(200, "OK")
        .case(404, "Not Found")
        // For complex logic, the pattern binds variables for the lambda body.
        // Note the lambda `=> { ... }` has no parameters of its own.
        .case(Err(e), => {
            log_error(e);
            "An unexpected error occurred"
        })
        .get();
7. Traits & Polymorphism

Polymorphism is the ability to write code that operates on values of different types, achieved through traits.

Static Polymorphism via Generics: Zero-cost, compile-time polymorphism.
private function log_as_json<T: Serializable>(item: T) {
    item.to_json() |> print()
}
Dynamic Polymorphism via Trait Objects: Runtime flexibility using dyn<Trait>.
let ui_elements: List<Box<dyn<Drawable>>> = [ ... ];
for element in ui_elements {
    element.draw(); // Call is dispatched at runtime
}
8. Concurrency

Async/Await:
private async function fetch_user_data(id: Uuid) -> User {
    http.get(f"/users/{id}").await()
}

// Async blocks
let result = async { 
    let data = fetch_data().await();
    process(data)
}.await();

Actor Model:
private actor Counter {
    count: int = 0;
    private handle Inc(amount: int) { self.count += amount; }
}

Channels and Message Passing:
// Create a channel
let ch = channel();

// Send and receive (using macro syntax)
send!(ch, "message");
let msg = recv!(ch);

// Spawn concurrent tasks
spawn {
    let result = expensive_computation();
    send!(ch, result);
};

let result = recv!(ch);
9. Effects and Side Effect Management

Effects provide a structured way to handle side effects. The perform keyword executes effectful operations:

// Perform I/O operations
perform IO.print("Hello, World");
perform IO.println("With newline");
let input = perform IO.read_line();

// State management
perform State.set(42);
let value = perform State.get();

// Other effect types
perform Network.fetch(url);
perform Random.int(1, 100);
perform Time.now();

// Handle effects with custom handlers
handle {
    perform IO.print("This will be captured");
    perform State.set(42);
} with {
    IO.print(msg) => captured_messages.push(msg),
    State.set(value) => { current_state = value }
}
10. Recursive Functions

Recursive functions can be defined using regular let bindings:
// Simple recursion
let factorial = (n) => {
    if (n <= 1) { 1 }
    else { n * factorial(n - 1) }
};

// Mutual recursion
let even = (n) => {
    if (n == 0) { true }
    else { odd(n - 1) }
};
let odd = (n) => {
    if (n == 0) { false }
    else { even(n - 1) }
};

Note: The current implementation may have limitations with recursive let bindings. 
For guaranteed support, use top-level function definitions.

11. Metaprogramming & Advanced Features

Derive Attributes:
private struct User { ... }.derive(Debug, Clone)

Contract Specifications (Future Feature):
@contract {
    requires: n >= 0,
    ensures: result >= 1
}
private function factorial(n: int) -> int { ... }

Macros:
// Macro invocation uses ! syntax
assert!(x > 0, "x must be positive");
println!("Value: {}", x);

// Custom macros (future feature)
macro rules! {
    // macro definition
}

FFI (Foreign Function Interface):
extern "C" {
    private function c_lib_function(input: i32) -> i32;
}

Effect Systems:
private effect Database { ... }
private function get_user(id: Uuid).with(Database) { ... }
12. Complete Example: Console Application

This example demonstrates a simple console application that fetches user data from a mock API, showcasing many of the language's features working together.

// main.flc

// === Definitions ===
public struct User { pub id: int, pub name: string }
    .derive(Debug, Clone)

public enum ApiError { NotFound, Network(string) }
    .derive(Debug)

public trait ApiClient {
    private async function fetch_user(self, id: int) -> Result<User, ApiError>;
}

private struct MockApiClient { db: HashMap<int, User> }

// === Implementations ===
private MockApiClient as ApiClient {
    private async function fetch_user(self, id: int) -> Result<User, ApiError> {
        tokio.sleep_ms(100).await();
        self.db.get(&id)
            .cloned()
            .ok_or(ApiError.NotFound)
    }
}

private MockApiClient {
    private function new() -> Self {
        let db = HashMap.new()
            .insert(1, User{id: 1, name: "Alice"})
            .insert(2, User{id: 2, name: "Bob"});
        Self{db: db}
    }
}

// === Logic Functions ===

// This function processes the data and returns a list of result strings.
private async function fetch_and_format_results(client: MockApiClient) -> List<string> {
    [1, 3, 2] // List of IDs to fetch
        .map_async(id => {
            client.fetch_user(id)
                .await()
                .match()
                    // The pattern `Ok({name, ..})` binds the `name` variable.
                    .case(Ok({name, ..}), => f"Success! Found user: {name}")
                    // The pattern `Err(...)` handles the NotFound case.
                    .case(Err(ApiError.NotFound), => f"Handled Error: User with ID '{id}' was not found.")
                    .get() // Get the resulting string from the match expression
        })
        .await() // Wait for all async map operations to complete
}

// This function takes the results and prints them to the console.
private function print_results(results: List<string>) {
    $("--- Processing Complete ---").print();
    results.for_each(result => $(result).print());
}

// === Main Application Entry Point ===
private async function main() {
    MockApiClient.new()
        .let(client => {
            fetch_and_format_results(client)
                .await()
                |> print_results()
        })
}

13. Multi-Module Example: Stock Portfolio Rebalancer

This example demonstrates how to structure a real application using FluentAI's module system.

// === File: Action.flc ===
module Action;

// Action types for portfolio rebalancing
public function Buy(symbol: string, shares: float, value: float) {
    {"type": "buy", "symbol": symbol, "shares": shares, "value": value}
}

public function Sell(symbol: string, shares: float, value: float) {
    {"type": "sell", "symbol": symbol, "shares": shares, "value": value}
}

public function Hold(symbol: string) {
    {"type": "hold", "symbol": symbol, "shares": 0.0, "value": 0.0}
}

// Helper functions
public function is_buy(action) -> bool {
    action.type == "buy"
}

public function is_sell(action) -> bool {
    action.type == "sell"
}

public function get_total_value(actions, type: string) -> float {
    actions
        .filter(a => a.type == type)
        .map(a => a.value)
        .reduce(0.0, (sum, val) => sum + val)
}

// === File: Stock.flc ===
module Stock;

public function Stock(symbol: string, shares: float, price: float) {
    // Private state
    let state = {
        "symbol": symbol,
        "shares": shares,
        "current_price": price
    };
    
    // Private validation
    let validate_shares = (shares) => shares >= 0.0;
    let validate_price = (price) => price > 0.0;
    
    // Public interface
    let self = {
        "get_symbol": () => state.symbol,
        "get_shares": () => state.shares,
        "get_price": () => state.current_price,
        "get_value": () => state.shares * state.current_price,
        
        "update_shares": (new_shares) => {
            if (validate_shares(new_shares)) {
                state.shares = new_shares;
            } else {
                $(f"Error: Invalid shares {new_shares}").print();
            }
            self
        },
        
        "update_price": (new_price) => {
            if (validate_price(new_price)) {
                state.current_price = new_price;
            }
            self
        },
        
        "to_data": () => {
            {"symbol": state.symbol, "shares": state.shares, "price": state.current_price}
        }
    };
    
    return self;
}

// === File: PriceService.flc ===
module PriceService;

public function PriceService() {
    // In a real app, this would fetch from an API
    let mock_prices = {
        "AAPL": 178.25,
        "GOOGL": 142.30,
        "MSFT": 195.96,
        "AMZN": 127.85,
        "TSLA": 245.50
    };
    
    let self = {
        "get_price": (symbol) => {
            mock_prices.get(symbol).unwrap_or(100.0)
        },
        
        "get_prices": (symbols) => {
            symbols.map(symbol => {
                {"symbol": symbol, "price": self.get_price(symbol)}
            })
        }
    };
    
    return self;
}

// === File: Portfolio.flc ===
module Portfolio;

use Stock;
use Action::*;  // Import all Action functions
use PriceService;

public function Portfolio() {
    // Private state
    let state = {
        "stocks": [],
        "allocations": [],
        "total_value": 0.0
    };
    
    let price_service = PriceService();
    
    // Private helpers
    let find_stock = (symbol) => {
        state.stocks.find(s => s.get_symbol() == symbol)
    };
    
    let find_allocation = (symbol) => {
        state.allocations.find(a => a.symbol == symbol)
    };
    
    let calculate_total_value = () => {
        state.stocks
            .map(s => s.get_value())
            .reduce(0.0, (sum, val) => sum + val)
    };
    
    // Public interface
    let self = {
        "add_stock": (symbol, shares) => {
            // Remove existing stock with same symbol
            state.stocks = state.stocks.filter(s => s.get_symbol() != symbol);
            
            // Add new stock
            let price = price_service.get_price(symbol);
            state.stocks.push(Stock(symbol, shares, price));
            
            self
        },
        
        "set_allocation": (symbol, percentage) => {
            if (percentage < 0.0 || percentage > 100.0) {
                $(f"Error: Invalid percentage {percentage}%").print();
                self
            } else {
                // Remove existing allocation
                state.allocations = state.allocations.filter(a => a.symbol != symbol);
                
                // Add new allocation
                state.allocations.push({"symbol": symbol, "target": percentage});
                
                self
            }
        },
        
        "update_prices": () => {
            state.stocks.for_each(stock => {
                let symbol = stock.get_symbol();
                let new_price = price_service.get_price(symbol);
                stock.update_price(new_price);
            });
            
            state.total_value = calculate_total_value();
            self
        },
        
        "validate": () => {
            let total = state.allocations
                .map(a => a.target)
                .reduce(0.0, (sum, pct) => sum + pct);
            
            let diff = (total - 100.0).abs();
            if (diff > 0.01) {
                $(f"Error: Allocations sum to {total}%, must equal 100%").print();
                false
            } else {
                true
            }
        },
        
        "calculate_rebalance": () => {
            if (!self.validate() || state.total_value == 0.0) {
                []
            } else {
                state.allocations.map(alloc => {
                    let symbol = alloc.symbol;
                    let target_pct = alloc.target;
                    let target_value = state.total_value * target_pct / 100.0;
                    
                    let stock = find_stock(symbol);
                    if (stock.is_some()) {
                        let s = stock.unwrap();
                        let current_value = s.get_value();
                        let diff = target_value - current_value;
                        
                        if (diff > 10.0) {
                            let shares = (diff / s.get_price()).floor();
                            Buy(symbol, shares, shares * s.get_price())
                        } else if (diff < -10.0) {
                            let shares = ((-diff) / s.get_price()).floor();
                            Sell(symbol, shares, shares * s.get_price())
                        } else {
                            Hold(symbol)
                        }
                    } else {
                        Hold(symbol)
                    }
                })
            }
        },
        
        "display": () => {
            $(f"Portfolio Value: ${state.total_value}").print();
            $("Current Holdings:").print();
            
            state.stocks.for_each(stock => {
                let symbol = stock.get_symbol();
                let shares = stock.get_shares();
                let price = stock.get_price();
                let value = stock.get_value();
                let pct = if (state.total_value > 0.0) {
                    (value / state.total_value * 100.0)
                } else { 0.0 };
                
                $(f"  {symbol}: {shares} @ ${price} = ${value} ({pct}%)").print();
            });
            
            self
        },
        
        "rebalance": () => {
            $("Fetching current prices...").print();
            self.update_prices();
            
            $("").print();
            $("=== Current Portfolio ===").print();
            self.display();
            
            $("").print();
            $("=== Rebalancing Actions ===").print();
            let actions = self.calculate_rebalance();
            
            actions.for_each(action => {
                if (is_buy(action)) {
                    $(f"BUY  {action.symbol}: {action.shares} shares (${action.value})").print();
                } else if (is_sell(action)) {
                    $(f"SELL {action.symbol}: {action.shares} shares (${action.value})").print();
                } else {
                    $(f"HOLD {action.symbol}: No action needed").print();
                }
            });
            
            let buy_total = get_total_value(actions, "buy");
            let sell_total = get_total_value(actions, "sell");
            
            $("").print();
            $("=== Summary ===").print();
            $(f"Total to buy:  ${buy_total}").print();
            $(f"Total to sell: ${sell_total}").print();
            $(f"Net cash flow: ${sell_total - buy_total}").print();
            
            self
        }
    };
    
    return self;
}

// === File: main.flc ===
use Portfolio;

private function main() {
    $("=== Stock Portfolio Rebalancer Demo ===").print();
    $("").print();
    
    // Create and configure portfolio using method chaining
    Portfolio()
        .add_stock("AAPL", 50.0)
        .add_stock("GOOGL", 10.0)
        .add_stock("MSFT", 25.0)
        .add_stock("AMZN", 15.0)
        .set_allocation("AAPL", 40.0)
        .set_allocation("GOOGL", 20.0)
        .set_allocation("MSFT", 25.0)
        .set_allocation("AMZN", 15.0)
        .rebalance();
    
    $("").print();
    $("Demo complete!").print();
}

// Run the application
main()

14. Current Limitations and Future Features

The following features are planned but not yet fully implemented:

Parser Limitations:
- Recursive let bindings may not work in all contexts
- Macro syntax (!) is recognized but custom macros cannot be defined
- Contract specifications (@contract) are not yet parsed
- Some async block constructs may not be fully supported

Future Features:
- Full macro system with macro rules!
- Contract-based programming with @contract
- More sophisticated pattern matching
- Algebraic effects beyond the current perform/handle system
- Compile-time reflection and code generation

Notes for Test Writers:
When writing tests, prefer using features that are fully documented and implemented.
Use #[ignore] attribute for tests that rely on future features.
