#!/bin/bash

echo "Debug Test for MCP HTTP Server"
echo "=============================="

# 1. Create a session
echo -e "\n1. Creating session..."
SESSION_RESPONSE=$(curl -s -X POST http://localhost:3000/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "client_info": {
      "name": "debug-test",
      "version": "1.0.0"
    }
  }')

echo "Raw Response: $SESSION_RESPONSE"

# Extract session ID
SESSION_ID=$(echo $SESSION_RESPONSE | sed -n 's/.*"session_id":"\([^"]*\)".*/\1/p')
echo "Extracted Session ID: $SESSION_ID"

# 2. Try to use the session
echo -e "\n2. Testing session endpoint..."
echo "URL: http://localhost:3000/sessions/$SESSION_ID/messages"

INIT_RESPONSE=$(curl -v -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
      "protocolVersion": "1.0",
      "clientInfo": {
        "name": "debug-test",
        "version": "1.0.0"
      }
    },
    "id": "init-1"
  }' 2>&1)

echo "Initialize Response:"
echo "$INIT_RESPONSE" | grep -A20 "< HTTP"