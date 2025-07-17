#!/usr/bin/env python3
import http.server
import socketserver
import os

PORT = 8080
DIRECTORY = "."

class CORSHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

    def do_GET(self):
        # Set correct MIME types
        if self.path.endswith('.wasm'):
            self.send_response(200)
            self.send_header('Content-Type', 'application/wasm')
            self.end_headers()
            with open(self.path[1:], 'rb') as f:
                self.wfile.write(f.read())
        else:
            super().do_GET()

os.chdir(DIRECTORY)

with socketserver.TCPServer(("", PORT), CORSHTTPRequestHandler) as httpd:
    print(f"🚀 DevTools showcase server running at http://localhost:{PORT}/")
    print(f"📂 Serving files from: {os.getcwd()}")
    print("\nOpen http://localhost:8080/devtools_showcase.html in your browser")
    print("Press Ctrl+C to stop the server")
    httpd.serve_forever()
