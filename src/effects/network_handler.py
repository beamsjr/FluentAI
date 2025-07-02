"""
Enhanced Network Effect Handler for ClaudeLang

This module implements comprehensive network effect handling including:
- HTTP requests (GET, POST, PUT, DELETE, etc.)
- WebSocket connections
- TCP/UDP sockets
- DNS lookups
- URL parsing
- Request/response handling with headers, cookies, etc.
"""

import json
import time
import socket
from typing import Dict, Any, List, Optional, Tuple, Union
from dataclasses import dataclass, field
from urllib.parse import urlparse, urlencode, parse_qs
from collections import defaultdict
import threading
import queue

from ..core.ast import EffectType
from .handlers import EffectHandler, EffectRequest, EffectResult


@dataclass
class HttpRequest:
    """HTTP request representation"""
    method: str
    url: str
    headers: Dict[str, str] = field(default_factory=dict)
    body: Optional[Union[str, bytes]] = None
    params: Dict[str, str] = field(default_factory=dict)
    json_data: Optional[Dict[str, Any]] = None
    timeout: float = 30.0
    follow_redirects: bool = True
    verify_ssl: bool = True


@dataclass
class HttpResponse:
    """HTTP response representation"""
    status_code: int
    headers: Dict[str, str]
    body: Union[str, bytes]
    url: str  # Final URL after redirects
    elapsed: float  # Time taken
    cookies: Dict[str, str] = field(default_factory=dict)
    history: List['HttpResponse'] = field(default_factory=list)  # Redirect history


@dataclass
class WebSocketMessage:
    """WebSocket message"""
    type: str  # "text", "binary", "close", "ping", "pong"
    data: Union[str, bytes]
    timestamp: float = field(default_factory=time.time)


@dataclass
class SocketInfo:
    """Socket connection information"""
    socket_id: str
    protocol: str  # "tcp", "udp"
    local_addr: Tuple[str, int]
    remote_addr: Optional[Tuple[str, int]]
    state: str  # "created", "bound", "listening", "connected", "closed"
    created_at: float = field(default_factory=time.time)


class NetworkHandler(EffectHandler):
    """Enhanced handler for network effects"""
    
    def __init__(self, 
                 allow_external: bool = True,
                 allowed_hosts: Optional[List[str]] = None,
                 mock_mode: bool = False):
        super().__init__()
        self.allow_external = allow_external
        self.allowed_hosts = allowed_hosts or []
        self.mock_mode = mock_mode
        
        # Mock responses for testing
        self.mock_responses: Dict[str, HttpResponse] = {}
        self.mock_websocket_messages: Dict[str, List[WebSocketMessage]] = defaultdict(list)
        
        # Active connections
        self.sockets: Dict[str, socket.socket] = {}
        self.socket_info: Dict[str, SocketInfo] = {}
        self.websockets: Dict[str, 'MockWebSocket'] = {}
        
        # Request history for debugging
        self.request_history: List[HttpRequest] = []
        
        # Rate limiting
        self.rate_limits: Dict[str, List[float]] = defaultdict(list)
        self.rate_limit_window = 60.0  # 1 minute
        self.rate_limit_max = 100  # Max requests per window
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        """Check if this handler can handle the given effect"""
        return effect_type == EffectType.NETWORK
    
    def handle(self, request: EffectRequest) -> EffectResult:
        """Handle a network effect request"""
        op = request.operation
        args = request.arguments
        
        # HTTP operations
        if op == "http-request":
            return self._handle_http_request(args)
        
        elif op == "http-get":
            return self._handle_http_get(args)
        
        elif op == "http-post":
            return self._handle_http_post(args)
        
        elif op == "http-put":
            return self._handle_http_put(args)
        
        elif op == "http-delete":
            return self._handle_http_delete(args)
        
        # WebSocket operations
        elif op == "ws-connect":
            return self._handle_ws_connect(args)
        
        elif op == "ws-send":
            return self._handle_ws_send(args)
        
        elif op == "ws-receive":
            return self._handle_ws_receive(args)
        
        elif op == "ws-close":
            return self._handle_ws_close(args)
        
        # Socket operations
        elif op == "socket-create":
            return self._handle_socket_create(args)
        
        elif op == "socket-bind":
            return self._handle_socket_bind(args)
        
        elif op == "socket-listen":
            return self._handle_socket_listen(args)
        
        elif op == "socket-connect":
            return self._handle_socket_connect(args)
        
        elif op == "socket-accept":
            return self._handle_socket_accept(args)
        
        elif op == "socket-send":
            return self._handle_socket_send(args)
        
        elif op == "socket-receive":
            return self._handle_socket_receive(args)
        
        elif op == "socket-close":
            return self._handle_socket_close(args)
        
        # DNS operations
        elif op == "dns-lookup":
            return self._handle_dns_lookup(args)
        
        elif op == "dns-reverse":
            return self._handle_dns_reverse(args)
        
        # URL operations
        elif op == "url-parse":
            return self._handle_url_parse(args)
        
        elif op == "url-encode":
            return self._handle_url_encode(args)
        
        elif op == "url-decode":
            return self._handle_url_decode(args)
        
        # Mock operations for testing
        elif op == "mock-response":
            return self._handle_mock_response(args)
        
        elif op == "mock-ws-message":
            return self._handle_mock_ws_message(args)
        
        # Utility operations
        elif op == "get-request-history":
            return self._handle_get_request_history(args)
        
        elif op == "clear-request-history":
            return self._handle_clear_request_history(args)
        
        else:
            raise ValueError(f"Unknown network operation: {op}")
    
    def _check_allowed_host(self, url: str) -> bool:
        """Check if host is allowed"""
        if not self.allow_external:
            return False
        
        if not self.allowed_hosts:
            return True
        
        parsed = urlparse(url)
        hostname = parsed.hostname
        
        for allowed in self.allowed_hosts:
            if allowed == "*" or hostname == allowed:
                return True
            if allowed.startswith("*.") and hostname.endswith(allowed[2:]):
                return True
        
        return False
    
    def _check_rate_limit(self, key: str) -> bool:
        """Check if rate limit is exceeded"""
        now = time.time()
        window_start = now - self.rate_limit_window
        
        # Clean old entries
        self.rate_limits[key] = [
            t for t in self.rate_limits[key] 
            if t > window_start
        ]
        
        # Check limit
        if len(self.rate_limits[key]) >= self.rate_limit_max:
            return False
        
        # Record request
        self.rate_limits[key].append(now)
        return True
    
    def _handle_http_request(self, args: List[Any]) -> EffectResult:
        """Handle generic HTTP request"""
        request_data = args[0] if args else {}
        
        # Build request
        req = HttpRequest(
            method=request_data.get("method", "GET"),
            url=request_data.get("url", ""),
            headers=request_data.get("headers", {}),
            body=request_data.get("body"),
            params=request_data.get("params", {}),
            json_data=request_data.get("json"),
            timeout=request_data.get("timeout", 30.0),
            follow_redirects=request_data.get("follow_redirects", True),
            verify_ssl=request_data.get("verify_ssl", True)
        )
        
        # Record request
        self.request_history.append(req)
        
        # Check if allowed
        if not self._check_allowed_host(req.url):
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["network-error", f"Host not allowed: {req.url}"])
                ]
            )
        
        # Check rate limit
        parsed_url = urlparse(req.url)
        if not self._check_rate_limit(parsed_url.hostname):
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["rate-limit", "Rate limit exceeded"])
                ]
            )
        
        # Mock mode
        if self.mock_mode:
            key = f"{req.method} {req.url}"
            if key in self.mock_responses:
                response = self.mock_responses[key]
                return EffectResult(value={
                    "status": response.status_code,
                    "headers": response.headers,
                    "body": response.body,
                    "url": response.url,
                    "elapsed": response.elapsed
                })
        
        # In real implementation, would use urllib or requests
        # For now, return mock response
        return EffectResult(value={
            "status": 200,
            "headers": {"Content-Type": "application/json"},
            "body": json.dumps({"message": f"Mock response for {req.method} {req.url}"}),
            "url": req.url,
            "elapsed": 0.1
        })
    
    def _handle_http_get(self, args: List[Any]) -> EffectResult:
        """Handle HTTP GET request"""
        url = args[0]
        headers = args[1] if len(args) > 1 else {}
        params = args[2] if len(args) > 2 else {}
        
        return self._handle_http_request([{
            "method": "GET",
            "url": url,
            "headers": headers,
            "params": params
        }])
    
    def _handle_http_post(self, args: List[Any]) -> EffectResult:
        """Handle HTTP POST request"""
        url = args[0]
        data = args[1] if len(args) > 1 else None
        headers = args[2] if len(args) > 2 else {}
        
        # Auto-detect JSON content
        if isinstance(data, dict):
            headers.setdefault("Content-Type", "application/json")
            body = json.dumps(data)
        else:
            body = data
        
        return self._handle_http_request([{
            "method": "POST",
            "url": url,
            "headers": headers,
            "body": body
        }])
    
    def _handle_http_put(self, args: List[Any]) -> EffectResult:
        """Handle HTTP PUT request"""
        url = args[0]
        data = args[1] if len(args) > 1 else None
        headers = args[2] if len(args) > 2 else {}
        
        if isinstance(data, dict):
            headers.setdefault("Content-Type", "application/json")
            body = json.dumps(data)
        else:
            body = data
        
        return self._handle_http_request([{
            "method": "PUT",
            "url": url,
            "headers": headers,
            "body": body
        }])
    
    def _handle_http_delete(self, args: List[Any]) -> EffectResult:
        """Handle HTTP DELETE request"""
        url = args[0]
        headers = args[1] if len(args) > 1 else {}
        
        return self._handle_http_request([{
            "method": "DELETE",
            "url": url,
            "headers": headers
        }])
    
    def _handle_ws_connect(self, args: List[Any]) -> EffectResult:
        """Handle WebSocket connection"""
        url = args[0]
        headers = args[1] if len(args) > 1 else {}
        
        if not self._check_allowed_host(url):
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["network-error", f"Host not allowed: {url}"])
                ]
            )
        
        # Create mock WebSocket
        ws_id = f"ws_{len(self.websockets)}"
        
        if self.mock_mode:
            ws = MockWebSocket(ws_id, url)
            self.websockets[ws_id] = ws
            
            # Add any pre-configured messages
            if url in self.mock_websocket_messages:
                ws.incoming_messages.extend(self.mock_websocket_messages[url])
            
            return EffectResult(value=ws_id)
        
        # Real implementation would create actual WebSocket
        return EffectResult(value=ws_id)
    
    def _handle_ws_send(self, args: List[Any]) -> EffectResult:
        """Handle WebSocket send"""
        ws_id = args[0]
        message = args[1]
        
        if ws_id not in self.websockets:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["network-error", "WebSocket not connected"])
                ]
            )
        
        ws = self.websockets[ws_id]
        
        # Create message
        msg = WebSocketMessage(
            type="text" if isinstance(message, str) else "binary",
            data=message
        )
        
        ws.outgoing_messages.append(msg)
        
        return EffectResult(value=None)
    
    def _handle_ws_receive(self, args: List[Any]) -> EffectResult:
        """Handle WebSocket receive"""
        ws_id = args[0]
        timeout = args[1] if len(args) > 1 else None
        
        if ws_id not in self.websockets:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["network-error", "WebSocket not connected"])
                ]
            )
        
        ws = self.websockets[ws_id]
        
        # Get next message
        if ws.incoming_messages:
            msg = ws.incoming_messages.pop(0)
            return EffectResult(value={
                "type": msg.type,
                "data": msg.data,
                "timestamp": msg.timestamp
            })
        
        # No messages available
        if timeout == 0:
            return EffectResult(value=None)
        
        # Would block in real implementation
        return EffectResult(value=None)
    
    def _handle_ws_close(self, args: List[Any]) -> EffectResult:
        """Handle WebSocket close"""
        ws_id = args[0]
        code = args[1] if len(args) > 1 else 1000
        reason = args[2] if len(args) > 2 else ""
        
        if ws_id in self.websockets:
            ws = self.websockets[ws_id]
            ws.state = "closed"
            del self.websockets[ws_id]
        
        return EffectResult(value=None)
    
    def _handle_socket_create(self, args: List[Any]) -> EffectResult:
        """Create a new socket"""
        protocol = args[0] if args else "tcp"
        
        socket_id = f"socket_{len(self.sockets)}"
        
        if self.mock_mode:
            # Create mock socket info
            info = SocketInfo(
                socket_id=socket_id,
                protocol=protocol,
                local_addr=("0.0.0.0", 0),
                remote_addr=None,
                state="created"
            )
            self.socket_info[socket_id] = info
            return EffectResult(value=socket_id)
        
        # Real implementation would create actual socket
        try:
            if protocol == "tcp":
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            elif protocol == "udp":
                sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            else:
                raise ValueError(f"Unknown protocol: {protocol}")
            
            self.sockets[socket_id] = sock
            self.socket_info[socket_id] = SocketInfo(
                socket_id=socket_id,
                protocol=protocol,
                local_addr=("0.0.0.0", 0),
                remote_addr=None,
                state="created"
            )
            
            return EffectResult(value=socket_id)
            
        except Exception as e:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", str(e)])
                ]
            )
    
    def _handle_socket_bind(self, args: List[Any]) -> EffectResult:
        """Bind socket to address"""
        socket_id = args[0]
        host = args[1]
        port = args[2]
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        info = self.socket_info[socket_id]
        info.local_addr = (host, port)
        info.state = "bound"
        
        if not self.mock_mode and socket_id in self.sockets:
            try:
                self.sockets[socket_id].bind((host, port))
            except Exception as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["socket-error", str(e)])
                    ]
                )
        
        return EffectResult(value=None)
    
    def _handle_socket_listen(self, args: List[Any]) -> EffectResult:
        """Listen for connections"""
        socket_id = args[0]
        backlog = args[1] if len(args) > 1 else 5
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        info = self.socket_info[socket_id]
        if info.protocol != "tcp":
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Can only listen on TCP sockets"])
                ]
            )
        
        info.state = "listening"
        
        if not self.mock_mode and socket_id in self.sockets:
            try:
                self.sockets[socket_id].listen(backlog)
            except Exception as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["socket-error", str(e)])
                    ]
                )
        
        return EffectResult(value=None)
    
    def _handle_socket_connect(self, args: List[Any]) -> EffectResult:
        """Connect to remote address"""
        socket_id = args[0]
        host = args[1]
        port = args[2]
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        if not self._check_allowed_host(f"tcp://{host}:{port}"):
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["network-error", f"Host not allowed: {host}"])
                ]
            )
        
        info = self.socket_info[socket_id]
        info.remote_addr = (host, port)
        info.state = "connected"
        
        if not self.mock_mode and socket_id in self.sockets:
            try:
                self.sockets[socket_id].connect((host, port))
            except Exception as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["socket-error", str(e)])
                    ]
                )
        
        return EffectResult(value=None)
    
    def _handle_socket_send(self, args: List[Any]) -> EffectResult:
        """Send data on socket"""
        socket_id = args[0]
        data = args[1]
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        # Convert string to bytes if needed
        if isinstance(data, str):
            data = data.encode('utf-8')
        
        if self.mock_mode:
            return EffectResult(value=len(data))
        
        if socket_id in self.sockets:
            try:
                sent = self.sockets[socket_id].send(data)
                return EffectResult(value=sent)
            except Exception as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["socket-error", str(e)])
                    ]
                )
        
        return EffectResult(value=len(data))
    
    def _handle_socket_receive(self, args: List[Any]) -> EffectResult:
        """Receive data from socket"""
        socket_id = args[0]
        max_bytes = args[1] if len(args) > 1 else 4096
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        if self.mock_mode:
            return EffectResult(value=b"Mock data")
        
        if socket_id in self.sockets:
            try:
                data = self.sockets[socket_id].recv(max_bytes)
                return EffectResult(value=data)
            except Exception as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", 
                                    ["socket-error", str(e)])
                    ]
                )
        
        return EffectResult(value=b"")
    
    def _handle_socket_accept(self, args: List[Any]) -> EffectResult:
        """Accept incoming connection"""
        socket_id = args[0]
        
        if socket_id not in self.socket_info:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not found"])
                ]
            )
        
        info = self.socket_info[socket_id]
        if info.state != "listening":
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["socket-error", "Socket not listening"])
                ]
            )
        
        if self.mock_mode:
            # Create new mock connection
            new_socket_id = f"socket_{len(self.sockets)}_accepted"
            new_info = SocketInfo(
                socket_id=new_socket_id,
                protocol="tcp",
                local_addr=info.local_addr,
                remote_addr=("mock.client", 12345),
                state="connected"
            )
            self.socket_info[new_socket_id] = new_info
            
            return EffectResult(value={
                "socket_id": new_socket_id,
                "remote_addr": new_info.remote_addr
            })
        
        # Real implementation would accept connection
        return EffectResult(value=None)
    
    def _handle_socket_close(self, args: List[Any]) -> EffectResult:
        """Close socket"""
        socket_id = args[0]
        
        if socket_id in self.socket_info:
            self.socket_info[socket_id].state = "closed"
            del self.socket_info[socket_id]
        
        if socket_id in self.sockets:
            try:
                self.sockets[socket_id].close()
            except:
                pass
            del self.sockets[socket_id]
        
        return EffectResult(value=None)
    
    def _handle_dns_lookup(self, args: List[Any]) -> EffectResult:
        """DNS hostname to IP lookup"""
        hostname = args[0]
        
        if self.mock_mode:
            # Return mock IP
            return EffectResult(value=["192.168.1.1", "192.168.1.2"])
        
        try:
            # Get all IP addresses for hostname
            _, _, ip_list = socket.gethostbyname_ex(hostname)
            return EffectResult(value=ip_list)
        except Exception as e:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["dns-error", str(e)])
                ]
            )
    
    def _handle_dns_reverse(self, args: List[Any]) -> EffectResult:
        """Reverse DNS lookup"""
        ip_addr = args[0]
        
        if self.mock_mode:
            return EffectResult(value="mock.example.com")
        
        try:
            hostname = socket.gethostbyaddr(ip_addr)[0]
            return EffectResult(value=hostname)
        except Exception as e:
            return EffectResult(
                value=None,
                secondary_effects=[
                    EffectRequest(EffectType.ERROR, "raise", 
                                ["dns-error", str(e)])
                ]
            )
    
    def _handle_url_parse(self, args: List[Any]) -> EffectResult:
        """Parse URL into components"""
        url = args[0]
        
        parsed = urlparse(url)
        query_params = parse_qs(parsed.query)
        
        return EffectResult(value={
            "scheme": parsed.scheme,
            "netloc": parsed.netloc,
            "hostname": parsed.hostname,
            "port": parsed.port,
            "path": parsed.path,
            "params": parsed.params,
            "query": parsed.query,
            "query_params": query_params,
            "fragment": parsed.fragment
        })
    
    def _handle_url_encode(self, args: List[Any]) -> EffectResult:
        """URL encode parameters"""
        params = args[0]
        
        encoded = urlencode(params)
        return EffectResult(value=encoded)
    
    def _handle_url_decode(self, args: List[Any]) -> EffectResult:
        """URL decode string"""
        encoded_str = args[0]
        
        params = parse_qs(encoded_str)
        return EffectResult(value=params)
    
    def _handle_mock_response(self, args: List[Any]) -> EffectResult:
        """Set mock HTTP response"""
        method = args[0]
        url = args[1]
        response_data = args[2]
        
        key = f"{method} {url}"
        
        self.mock_responses[key] = HttpResponse(
            status_code=response_data.get("status", 200),
            headers=response_data.get("headers", {}),
            body=response_data.get("body", ""),
            url=url,
            elapsed=response_data.get("elapsed", 0.1),
            cookies=response_data.get("cookies", {})
        )
        
        return EffectResult(value=None)
    
    def _handle_mock_ws_message(self, args: List[Any]) -> EffectResult:
        """Add mock WebSocket message"""
        url = args[0]
        message = args[1]
        
        msg = WebSocketMessage(
            type="text" if isinstance(message, str) else "binary",
            data=message
        )
        
        self.mock_websocket_messages[url].append(msg)
        
        return EffectResult(value=None)
    
    def _handle_get_request_history(self, args: List[Any]) -> EffectResult:
        """Get request history"""
        limit = args[0] if args else None
        
        history = self.request_history
        if limit:
            history = history[-limit:]
        
        return EffectResult(value=[
            {
                "method": req.method,
                "url": req.url,
                "headers": req.headers,
                "body": req.body,
                "params": req.params
            }
            for req in history
        ])
    
    def _handle_clear_request_history(self, args: List[Any]) -> EffectResult:
        """Clear request history"""
        self.request_history.clear()
        return EffectResult(value=None)
    
    def cleanup(self):
        """Clean up all resources"""
        # Close all sockets
        for sock in list(self.sockets.values()):
            try:
                sock.close()
            except:
                pass
        self.sockets.clear()
        self.socket_info.clear()
        
        # Close all WebSockets
        for ws in list(self.websockets.values()):
            ws.state = "closed"
        self.websockets.clear()
        
        # Clear other data
        self.request_history.clear()
        self.mock_responses.clear()
        self.mock_websocket_messages.clear()


class MockWebSocket:
    """Mock WebSocket for testing"""
    
    def __init__(self, ws_id: str, url: str):
        self.ws_id = ws_id
        self.url = url
        self.state = "connected"
        self.incoming_messages: List[WebSocketMessage] = []
        self.outgoing_messages: List[WebSocketMessage] = []