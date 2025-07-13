# Stock Portfolio Rebalancer - Modular Version

This is a modular implementation of the stock portfolio rebalancer using FluentAI's new module system with FLC (Fluent Lambda Chain) syntax.

## Module Structure

The application is organized into several modules, each with a specific responsibility:

### Core Modules

- **Types.flc** - Core data structures and type constructors
  - Stock representation with symbol, shares, and price
  - Allocation targets for rebalancing
  - Portfolio container
  - Action types (buy/sell/hold)
  - Result types for error handling

- **PriceService.flc** - Stock price fetching service
  - Simulates fetching real-time stock prices
  - Provides price updates for individual stocks
  - Batch price fetching for multiple symbols

- **Portfolio.flc** - Portfolio management
  - Add/remove stocks from portfolio
  - Set target allocation percentages
  - Calculate current allocations
  - Update portfolio values
  - Validate allocation totals

- **Rebalancer.flc** - Rebalancing calculations
  - Calculate buy/sell actions to reach target allocations
  - Generate detailed rebalancing reports
  - Summarize total buy/sell values
  - Format actions for display

- **UI.flc** - User interface and command processing
  - Interactive command-line interface
  - Command parsing and dispatch
  - Portfolio display formatting
  - Help system

### Main Application

- **main_new.flc** - Application entry point
  - Initializes the application
  - Runs the main command loop
  - Handles demo mode for testing

## Key Features

1. **Modular Design**: Each module has a clear, single responsibility
2. **Type Safety**: Uses typed function signatures throughout
3. **Error Handling**: Consistent error handling with Result types
4. **Immutable Data**: All operations return new data structures
5. **Functional Style**: Uses map, filter, reduce, and other functional operations

## FLC Syntax Features Used

- **Module System**: Each file is a module with automatic exports for public functions
- **Import Statements**: `use ModuleName;` to import module functionality
- **Function Types**: Full type annotations on functions
- **Pattern Matching**: `match` expressions for control flow
- **Method Chaining**: Fluent API style for data transformations
- **String Interpolation**: F-strings for formatted output
- **Lambda Expressions**: `param => expression` syntax throughout

## Example Usage

```flc
// Import required modules
use Types;
use Portfolio;
use Rebalancer;

// Create a new portfolio
let portfolio = Types.create_portfolio();

// Add stocks
portfolio = Portfolio.add_stock(portfolio, "AAPL", 50.0);
portfolio = Portfolio.add_stock(portfolio, "GOOGL", 10.0);

// Set target allocations
portfolio = Portfolio.set_allocation(portfolio, "AAPL", 60.0);
portfolio = Portfolio.set_allocation(portfolio, "GOOGL", 40.0);

// Calculate rebalancing actions
let result = Rebalancer.calculate_rebalance_actions(portfolio);
```

## Running the Application

```bash
# Run interactive mode
fluentai run main_new.flc

# Run with demo portfolio
fluentai run main_new.flc -- --demo
```

## Module Benefits

This modular architecture provides several benefits:

1. **Separation of Concerns**: Each module handles one aspect of the application
2. **Testability**: Individual modules can be tested in isolation
3. **Reusability**: Modules can be imported and used in other applications
4. **Maintainability**: Changes to one module don't affect others
5. **Clarity**: Clear module boundaries make the code easier to understand

## Future Enhancements

When FluentAI's module system is fully implemented, additional features could include:

- Real HTTP client for fetching live stock prices
- Database persistence for portfolio data
- More sophisticated rebalancing algorithms
- Tax optimization strategies
- Historical performance tracking
- Export functionality for reports