#!/bin/bash

echo "Quick MCP HTTP Server Test"
echo "========================="

# 1. Create a session
echo -e "\n1. Creating session..."
SESSION_RESPONSE=$(curl -s -X POST http://localhost:3000/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "client_info": {
      "name": "quick-test",
      "version": "1.0.0"
    }
  }')

echo "Response: $SESSION_RESPONSE"

# Extract session ID using a more robust method
SESSION_ID=$(echo $SESSION_RESPONSE | sed -n 's/.*"session_id":"\([^"]*\)".*/\1/p')
echo "Session ID: $SESSION_ID"

if [ -z "$SESSION_ID" ]; then
    echo "Failed to get session ID!"
    exit 1
fi

# 2. Initialize
echo -e "\n2. Initializing..."
curl -s -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
      "protocolVersion": "1.0",
      "clientInfo": {
        "name": "quick-test",
        "version": "1.0.0"
      }
    },
    "id": "init-1"
  }' | jq .

# 3. List tools
echo -e "\n3. Listing tools..."
curl -s -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/list",
    "id": "tools-1"
  }' | jq .

# 4. Execute some code
echo -e "\n4. Executing (+ 1 2 3)..."
curl -s -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
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
  }' | jq .

echo -e "\nYou can test SSE in your browser at:"
echo "http://localhost:3000/sessions/$SESSION_ID/sse"