#!/usr/bin/env python3
"""
Simple HTTP server for serving FluentAI renderer demos with proper MIME types.
Ensures WASM files are served with the correct content type.
"""

import http.server
import socketserver
import os
import sys
from pathlib import Path

PORT = 8082

class WASMHandler(http.server.SimpleHTTPRequestHandler):
    """Custom handler that serves WASM files with correct MIME type"""
    
    def end_headers(self):
        # Add CORS headers for local development
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        super().end_headers()
    
    def guess_type(self, path):
        """Guess the MIME type of a file with custom handling for WASM"""
        mimetype, _ = super().guess_type(path)
        
        # Custom MIME types
        if path.endswith('.wasm'):
            return 'application/wasm'
        elif path.endswith('.js') and 'fluentai' in path:
            # Ensure JS modules are served with correct type
            return 'application/javascript'
        
        return mimetype

def main():
    """Run the demo server"""
    # Change to the renderer directory
    script_dir = Path(__file__).parent
    os.chdir(script_dir)
    
    print(f"Starting FluentAI Renderer Demo Server on http://localhost:{PORT}")
    print(f"Serving from: {os.getcwd()}")
    print("\nAvailable demos:")
    print(f"  - Demo Index: http://localhost:{PORT}/demos.html")
    print(f"  - Main Demo: http://localhost:{PORT}/www/index.html")
    print(f"  - AR Demo: http://localhost:{PORT}/examples/ar_demo.html")
    print("\nNote: The existing serve_demo.py runs on port 8081")
    print("Press Ctrl+C to stop the server")
    
    with socketserver.TCPServer(("", PORT), WASMHandler) as httpd:
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nShutting down server...")
            sys.exit(0)

if __name__ == "__main__":
    main()