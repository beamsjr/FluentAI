#!/bin/bash

# Test script for HTTP server functionality

echo "FluentAI MCP HTTP Server Test"
echo "============================="

# Start the server in the background
echo "Starting MCP server in HTTP mode..."
cargo run --bin fluentai-mcp -- --transport http --port 3333 &
SERVER_PID=$!

# Wait for server to start
sleep 2

# Test 1: Create session
echo -e "\n1. Testing session creation..."
SESSION_RESPONSE=$(curl -s -X POST http://localhost:3333/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "client_info": {
      "name": "test-client",
      "version": "1.0.0"
    }
  }')

echo "Response: $SESSION_RESPONSE"

# Extract session ID
SESSION_ID=$(echo $SESSION_RESPONSE | grep -o '"session_id":"[^"]*' | cut -d'"' -f4)
echo "Session ID: $SESSION_ID"

# Test 2: Initialize
echo -e "\n2. Testing initialize..."
INIT_RESPONSE=$(curl -s -X POST "http://localhost:3333/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
      "protocolVersion": "1.0",
      "clientInfo": {
        "name": "test-client",
        "version": "1.0.0"
      }
    },
    "id": "init-1"
  }')

echo "Response: $INIT_RESPONSE"

# Test 3: List tools
echo -e "\n3. Testing tools/list..."
TOOLS_RESPONSE=$(curl -s -X POST "http://localhost:3333/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/list",
    "id": "tools-1"
  }')

echo "Response: $TOOLS_RESPONSE"

# Test 4: Eval tool
echo -e "\n4. Testing eval tool..."
EVAL_RESPONSE=$(curl -s -X POST "http://localhost:3333/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "eval",
      "arguments": {
        "code": "(+ 1 2 3)"
      }
    },
    "id": "eval-1"
  }')

echo "Response: $EVAL_RESPONSE"

# Cleanup
echo -e "\n5. Shutting down server..."
kill $SERVER_PID
wait $SERVER_PID 2>/dev/null

echo -e "\nTest completed!"