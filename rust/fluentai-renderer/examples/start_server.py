#!/usr/bin/env python3
import http.server
import socketserver
import os

PORT = 8080

class CORSHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()
    
    def guess_type(self, path):
        mimetype, _ = super().guess_type(path)
        if path.endswith('.wasm'):
            return 'application/wasm'
        return mimetype

print(f"Starting server at http://localhost:{PORT}")
print(f"Open http://localhost:{PORT}/3d_demo_real.html to see the FluentAI 3D WASM demo")
print("Press Ctrl+C to stop the server")

os.chdir(os.path.dirname(__file__))
with socketserver.TCPServer(("", PORT), CORSHTTPRequestHandler) as httpd:
    httpd.serve_forever()