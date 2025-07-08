#!/bin/bash

# Create a session
SESSION_RESPONSE=$(curl -s -X POST http://localhost:3000/sessions \
  -H "Content-Type: application/json" \
  -d '{"client_info": {"name": "test", "version": "1.0"}}')

SESSION_ID=$(echo $SESSION_RESPONSE | python3 -c "import json, sys; print(json.load(sys.stdin)['session_id'])")

echo "Session ID: $SESSION_ID"

# Initialize the session
INIT_RESPONSE=$(curl -s -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
      "protocolVersion": "2024-07-22",
      "capabilities": {},
      "clientInfo": {
        "name": "Test Client",
        "version": "1.0"
      }
    },
    "id": "init-1"
  }')

echo "Initialize response:"
echo $INIT_RESPONSE | python3 -m json.tool

# Search for "subtraction"
SEARCH_RESPONSE=$(curl -s -X POST "http://localhost:3000/sessions/$SESSION_ID/messages" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "search_docs",
      "arguments": {
        "query": "subtraction"
      }
    },
    "id": "search-1"
  }')

echo -e "\nSearch response for 'subtraction':"
echo $SEARCH_RESPONSE | python3 -m json.tool | head -50