#!/usr/bin/env python3
"""
Simple HTTP server for FluentAI renderer demos
Serves all files with proper MIME types for WASM
"""

import http.server
import socketserver
import os

PORT = 8081

# Change to the renderer directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

print(f"Starting server at http://localhost:{PORT}/")
print(f"Serving from: {os.getcwd()}")
print(f"\nDemo Index: http://localhost:{PORT}/examples/demos.html")
print(f"\nPress Ctrl+C to stop the server")

# Use the built-in simple HTTP server
handler = http.server.SimpleHTTPRequestHandler

# Monkey patch the extensions_map to add WASM support
handler.extensions_map['.wasm'] = 'application/wasm'

with socketserver.TCPServer(("", PORT), handler) as httpd:
    httpd.serve_forever()