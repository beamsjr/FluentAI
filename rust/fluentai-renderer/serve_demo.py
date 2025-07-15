#!/usr/bin/env python3
import http.server
import socketserver
import os

PORT = 8081

class MyHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        # Add CORS headers for WASM
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()
    
    def guess_type(self, path):
        mimetype = super().guess_type(path)
        if path.endswith('.wasm'):
            return 'application/wasm'
        return mimetype

os.chdir(os.path.dirname(os.path.abspath(__file__)))

with socketserver.TCPServer(("", PORT), MyHTTPRequestHandler) as httpd:
    print(f"Server running at http://localhost:{PORT}/")
    print(f"\nAvailable demos:")
    print(f"  - Demo Index: http://localhost:{PORT}/examples/demos.html")
    print(f"  - Main Demo: http://localhost:{PORT}/www/index.html")
    print(f"  - AR Demo: http://localhost:{PORT}/examples/ar_demo.html")
    print(f"  - Phase 2: http://localhost:{PORT}/www/phase2_demo.html")
    print(f"  - Phase 3: http://localhost:{PORT}/www/phase3_demo.html")
    print(f"  - Phase 4: http://localhost:{PORT}/www/phase4_demo.html")
    print(f"\nPress Ctrl+C to stop the server")
    httpd.serve_forever()