# Stock Portfolio Rebalancer

A FluentAI application for calculating stock portfolio rebalancing actions based on target allocations.

## Features

- Add stocks to your portfolio with current share counts
- Set target allocation percentages for each stock
- Fetch real-time stock prices from Yahoo Finance
- Calculate buy/sell actions needed to reach target allocations
- Interactive command-line interface

## Usage

### Interactive Mode

Run the application and use the following commands:

```
add <symbol> <shares>     - Add a stock to your portfolio
alloc <symbol> <percent>  - Set target allocation percentage
rebalance                 - Calculate rebalancing actions
portfolio                 - Show current portfolio
help                      - Show help message
quit                      - Exit the application
```

### Example Session

```
> add AAPL 50
Added 50.0 shares of AAPL

> add GOOGL 10
Added 10.0 shares of GOOGL

> add MSFT 25
Added 25.0 shares of MSFT

> alloc AAPL 40
Set allocation for AAPL to 40%

> alloc GOOGL 30
Set allocation for GOOGL to 30%

> alloc MSFT 30
Set allocation for MSFT to 30%

> rebalance
Fetching current prices...

=== Current Portfolio ===
Portfolio Value: $15234.50

Current Holdings:
  AAPL: 50.00 shares @ $178.25 = $8912.50 (58.5%)
  GOOGL: 10.00 shares @ $142.30 = $1423.00 (9.3%)
  MSFT: 25.00 shares @ $195.96 = $4899.00 (32.1%)

Target Allocations:
  AAPL: 40.0%
  GOOGL: 30.0%
  MSFT: 30.0%

=== Rebalancing Actions ===
SELL AAPL: 15.79 shares ($2818.70)
BUY  GOOGL: 22.13 shares ($3147.35)
SELL MSFT: 1.68 shares ($328.65)

=== Summary ===
Total to buy:  $3147.35
Total to sell: $3147.35
Net cash flow: $0.00
```

## Implementation Details

The application consists of several modules:

- `types.fc` - Core data structures and error types
- `yahoo_api.fc` - Yahoo Finance API client for fetching stock prices
- `portfolio_manager.fc` - Portfolio management and state tracking
- `rebalancer.fc` - Rebalancing calculation and display logic
- `main.fc` - Main application and user interface

## Notes

- Allocations must sum to exactly 100%
- The application assumes you can buy fractional shares
- Prices are fetched in real-time from Yahoo Finance
- Network errors are handled gracefully with appropriate error messages