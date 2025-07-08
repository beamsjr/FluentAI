# FluentAI MCP Server Environment Variables

The FluentAI MCP server supports the following environment variables for configuration:

## Security Configuration

### `FLUENTAI_MAX_MEMORY_MB`
- **Description**: Maximum memory allocation allowed for the VM in megabytes
- **Default**: `50` (50MB)
- **Example**: `FLUENTAI_MAX_MEMORY_MB=100`

### `FLUENTAI_MAX_INSTRUCTIONS`
- **Description**: Maximum number of instructions the VM can execute
- **Default**: `10000000` (10 million)
- **Example**: `FLUENTAI_MAX_INSTRUCTIONS=50000000`

### `FLUENTAI_MAX_ALLOCATIONS`
- **Description**: Maximum number of memory allocations allowed
- **Default**: `50000` (50 thousand)
- **Example**: `FLUENTAI_MAX_ALLOCATIONS=100000`

### `FLUENTAI_EXECUTION_TIMEOUT_SECS`
- **Description**: Maximum execution time for code in seconds
- **Default**: `30` (30 seconds)
- **Example**: `FLUENTAI_EXECUTION_TIMEOUT_SECS=60`

## Usage Example

```bash
# Run with custom limits
FLUENTAI_MAX_MEMORY_MB=100 \
FLUENTAI_MAX_INSTRUCTIONS=50000000 \
FLUENTAI_EXECUTION_TIMEOUT_SECS=60 \
cargo run --bin fluentai-mcp -- http
```

## Security Notes

These limits are designed to prevent:
- Memory exhaustion attacks
- Infinite loops
- Excessive resource consumption

The default values are suitable for demonstration and development. For production use, adjust these values based on your specific requirements and available resources.