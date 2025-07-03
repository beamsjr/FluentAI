# ClaudeLang MCP Server

An MCP (Model Context Protocol) server that provides ClaudeLang language tools and documentation.

## Features

The MCP server exposes the following tools:

### `eval`
Execute ClaudeLang code and return results.

**Parameters:**
- `code` (string): ClaudeLang code to execute

**Example:**
```json
{
  "name": "eval",
  "arguments": {
    "code": "(+ 1 2)"
  }
}
```

### `search_docs`
Search documentation for a query.

**Parameters:**
- `query` (string): Search query

**Example:**
```json
{
  "name": "search_docs",
  "arguments": {
    "query": "lambda"
  }
}
```

### `get_syntax`
Get documentation for a specific construct.

**Parameters:**
- `name` (string): Name of the construct

**Example:**
```json
{
  "name": "get_syntax",
  "arguments": {
    "name": "If"
  }
}
```

### `list_features`
List all language features grouped by category.

**Parameters:** None

### `reset_interpreter`
Clear the interpreter state.

**Parameters:** None

## Running the Server

```bash
cargo run --bin claudelang-mcp
```

The server listens on stdin/stdout following the MCP protocol.

## Using with Claude Desktop

To configure the ClaudeLang MCP server as a local tool for Claude Desktop:

1. Build your binary:
   ```bash
   cargo build --release --bin claudelang-mcp
   ```

2. Create a `claude-tools.json` file in the same directory as your compiled binary:
   ```json
   {
     "name": "ClaudeLang",
     "command": "./claude-lang-mcp",
     "protocol": "stdio"
   }
   ```

   > Replace `./claude-lang-mcp` with the correct relative path to your binary.

3. Launch Claude Desktop. It should detect your tool and make it available under "Local Tools."

4. You can now call any tool (e.g., `eval`, `search_docs`) from within Claude Desktop.

## Documentation Coverage

The server provides comprehensive documentation for:

- **Literals**: Integer, Float, String, Boolean, Nil
- **Variables**: Variable, QualifiedVariable
- **Functions**: Lambda, Application, Let, Letrec
- **Control Flow**: If, Match
- **Effects**: Effect operations for IO, State, Error, etc.
- **Data Structures**: List
- **Modules**: Module, Import, Export
- **Async/Concurrent**: Async, Await, Spawn, Channel, Send, Receive
- **Operators**: Arithmetic (+, -, *, /), Comparison (=, !=, <, >), Logical (and, or, not)
- **Keywords**: All language keywords with syntax and examples

## Example Usage

When connected via MCP, you can:

1. Execute code:
   ```
   eval: "(let ((x 5)) (+ x 10))"
   Result: 15
   ```

2. Search for documentation:
   ```
   search_docs: "function"
   Results: Lambda, Application, Let, Letrec, etc.
   ```

3. Get specific syntax help:
   ```
   get_syntax: "Lambda"
   Result: Detailed documentation with examples
   ```
