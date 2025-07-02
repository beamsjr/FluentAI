"""
Test Network Effect Handler
"""

import unittest
import json
from unittest.mock import Mock, patch

from src.core.ast import EffectType
from src.effects.network_handler import (
    NetworkHandler, HttpRequest, HttpResponse, WebSocketMessage,
    SocketInfo, MockWebSocket
)
from src.effects.handlers import EffectRequest, EffectResult


class TestNetworkHandler(unittest.TestCase):
    """Test network effect handler"""
    
    def setUp(self):
        self.handler = NetworkHandler(mock_mode=True)
    
    def tearDown(self):
        self.handler.cleanup()
    
    def test_http_get_request(self):
        """Test HTTP GET request"""
        request = EffectRequest(
            EffectType.NETWORK,
            "http-get",
            ["https://example.com/api"]
        )
        
        result = self.handler.handle(request)
        
        self.assertIsInstance(result, EffectResult)
        self.assertIsInstance(result.value, dict)
        self.assertIn("status", result.value)
        self.assertIn("body", result.value)
        self.assertEqual(result.value["status"], 200)
    
    def test_http_post_request(self):
        """Test HTTP POST request with JSON data"""
        data = {"name": "test", "value": 123}
        request = EffectRequest(
            EffectType.NETWORK,
            "http-post",
            ["https://example.com/api", data]
        )
        
        result = self.handler.handle(request)
        
        self.assertIsInstance(result, EffectResult)
        self.assertEqual(result.value["status"], 200)
        
        # Check request was recorded
        self.assertEqual(len(self.handler.request_history), 1)
        recorded = self.handler.request_history[0]
        self.assertEqual(recorded.method, "POST")
        self.assertEqual(recorded.url, "https://example.com/api")
    
    def test_http_with_headers(self):
        """Test HTTP request with custom headers"""
        headers = {"Authorization": "Bearer token123"}
        request = EffectRequest(
            EffectType.NETWORK,
            "http-get",
            ["https://example.com/api", headers]
        )
        
        result = self.handler.handle(request)
        
        self.assertEqual(result.value["status"], 200)
        recorded = self.handler.request_history[0]
        self.assertEqual(recorded.headers["Authorization"], "Bearer token123")
    
    def test_mock_response(self):
        """Test mock response functionality"""
        # Set up mock
        mock_request = EffectRequest(
            EffectType.NETWORK,
            "mock-response",
            ["GET", "https://test.com/api", {
                "status": 404,
                "body": "Not found",
                "headers": {"Content-Type": "text/plain"}
            }]
        )
        self.handler.handle(mock_request)
        
        # Make request
        request = EffectRequest(
            EffectType.NETWORK,
            "http-request",
            [{"method": "GET", "url": "https://test.com/api"}]
        )
        result = self.handler.handle(request)
        
        self.assertEqual(result.value["status"], 404)
        self.assertEqual(result.value["body"], "Not found")
    
    def test_websocket_connect(self):
        """Test WebSocket connection"""
        request = EffectRequest(
            EffectType.NETWORK,
            "ws-connect",
            ["wss://example.com/ws"]
        )
        
        result = self.handler.handle(request)
        
        self.assertIsNotNone(result.value)
        ws_id = result.value
        self.assertIn(ws_id, self.handler.websockets)
    
    def test_websocket_send_receive(self):
        """Test WebSocket send and receive"""
        # Connect
        connect_req = EffectRequest(
            EffectType.NETWORK,
            "ws-connect",
            ["wss://example.com/ws"]
        )
        connect_result = self.handler.handle(connect_req)
        ws_id = connect_result.value
        
        # Add mock incoming message
        ws = self.handler.websockets[ws_id]
        ws.incoming_messages.append(
            WebSocketMessage(type="text", data="Hello from server")
        )
        
        # Send message
        send_req = EffectRequest(
            EffectType.NETWORK,
            "ws-send",
            [ws_id, "Hello from client"]
        )
        self.handler.handle(send_req)
        
        # Check outgoing message was recorded
        self.assertEqual(len(ws.outgoing_messages), 1)
        self.assertEqual(ws.outgoing_messages[0].data, "Hello from client")
        
        # Receive message
        receive_req = EffectRequest(
            EffectType.NETWORK,
            "ws-receive",
            [ws_id]
        )
        receive_result = self.handler.handle(receive_req)
        
        self.assertIsNotNone(receive_result.value)
        self.assertEqual(receive_result.value["data"], "Hello from server")
    
    def test_websocket_close(self):
        """Test WebSocket close"""
        # Connect
        connect_req = EffectRequest(
            EffectType.NETWORK,
            "ws-connect",
            ["wss://example.com/ws"]
        )
        connect_result = self.handler.handle(connect_req)
        ws_id = connect_result.value
        
        # Close
        close_req = EffectRequest(
            EffectType.NETWORK,
            "ws-close",
            [ws_id]
        )
        self.handler.handle(close_req)
        
        self.assertNotIn(ws_id, self.handler.websockets)
    
    def test_socket_create(self):
        """Test socket creation"""
        request = EffectRequest(
            EffectType.NETWORK,
            "socket-create",
            ["tcp"]
        )
        
        result = self.handler.handle(request)
        
        socket_id = result.value
        self.assertIsNotNone(socket_id)
        self.assertIn(socket_id, self.handler.socket_info)
        
        info = self.handler.socket_info[socket_id]
        self.assertEqual(info.protocol, "tcp")
        self.assertEqual(info.state, "created")
    
    def test_socket_bind_listen(self):
        """Test socket bind and listen"""
        # Create socket
        create_req = EffectRequest(
            EffectType.NETWORK,
            "socket-create",
            ["tcp"]
        )
        create_result = self.handler.handle(create_req)
        socket_id = create_result.value
        
        # Bind
        bind_req = EffectRequest(
            EffectType.NETWORK,
            "socket-bind",
            [socket_id, "0.0.0.0", 8888]
        )
        self.handler.handle(bind_req)
        
        info = self.handler.socket_info[socket_id]
        self.assertEqual(info.local_addr, ("0.0.0.0", 8888))
        self.assertEqual(info.state, "bound")
        
        # Listen
        listen_req = EffectRequest(
            EffectType.NETWORK,
            "socket-listen",
            [socket_id, 5]
        )
        self.handler.handle(listen_req)
        
        self.assertEqual(info.state, "listening")
    
    def test_socket_connect(self):
        """Test socket connect"""
        # Create socket
        create_req = EffectRequest(
            EffectType.NETWORK,
            "socket-create",
            ["tcp"]
        )
        create_result = self.handler.handle(create_req)
        socket_id = create_result.value
        
        # Connect
        connect_req = EffectRequest(
            EffectType.NETWORK,
            "socket-connect",
            [socket_id, "example.com", 80]
        )
        self.handler.handle(connect_req)
        
        info = self.handler.socket_info[socket_id]
        self.assertEqual(info.remote_addr, ("example.com", 80))
        self.assertEqual(info.state, "connected")
    
    def test_socket_send_receive(self):
        """Test socket send and receive"""
        # Create connected socket
        create_req = EffectRequest(
            EffectType.NETWORK,
            "socket-create",
            ["tcp"]
        )
        create_result = self.handler.handle(create_req)
        socket_id = create_result.value
        
        # Send data
        send_req = EffectRequest(
            EffectType.NETWORK,
            "socket-send",
            [socket_id, "Hello, socket!"]
        )
        send_result = self.handler.handle(send_req)
        
        # In mock mode, returns length of data
        self.assertEqual(send_result.value, 14)
        
        # Receive data
        receive_req = EffectRequest(
            EffectType.NETWORK,
            "socket-receive",
            [socket_id, 1024]
        )
        receive_result = self.handler.handle(receive_req)
        
        # In mock mode, returns mock data
        self.assertEqual(receive_result.value, b"Mock data")
    
    def test_dns_lookup(self):
        """Test DNS lookup"""
        request = EffectRequest(
            EffectType.NETWORK,
            "dns-lookup",
            ["example.com"]
        )
        
        result = self.handler.handle(request)
        
        self.assertIsInstance(result.value, list)
        self.assertTrue(all(isinstance(ip, str) for ip in result.value))
    
    def test_dns_reverse(self):
        """Test reverse DNS lookup"""
        request = EffectRequest(
            EffectType.NETWORK,
            "dns-reverse",
            ["192.168.1.1"]
        )
        
        result = self.handler.handle(request)
        
        self.assertIsInstance(result.value, str)
        self.assertEqual(result.value, "mock.example.com")
    
    def test_url_parse(self):
        """Test URL parsing"""
        request = EffectRequest(
            EffectType.NETWORK,
            "url-parse",
            ["https://user:pass@example.com:8080/path?key=value&foo=bar#section"]
        )
        
        result = self.handler.handle(request)
        parsed = result.value
        
        self.assertEqual(parsed["scheme"], "https")
        self.assertEqual(parsed["hostname"], "example.com")
        self.assertEqual(parsed["port"], 8080)
        self.assertEqual(parsed["path"], "/path")
        self.assertEqual(parsed["query"], "key=value&foo=bar")
        self.assertEqual(parsed["fragment"], "section")
        self.assertIn("key", parsed["query_params"])
        self.assertIn("foo", parsed["query_params"])
    
    def test_url_encode_decode(self):
        """Test URL encoding and decoding"""
        # Encode
        params = {"name": "John Doe", "email": "john@example.com"}
        encode_req = EffectRequest(
            EffectType.NETWORK,
            "url-encode",
            [params]
        )
        encode_result = self.handler.handle(encode_req)
        
        self.assertIn("name=John+Doe", encode_result.value)
        self.assertIn("email=john%40example.com", encode_result.value)
        
        # Decode
        decode_req = EffectRequest(
            EffectType.NETWORK,
            "url-decode",
            [encode_result.value]
        )
        decode_result = self.handler.handle(decode_req)
        
        self.assertEqual(decode_result.value["name"], ["John Doe"])
        self.assertEqual(decode_result.value["email"], ["john@example.com"])
    
    def test_request_history(self):
        """Test request history tracking"""
        # Make several requests
        for i in range(5):
            request = EffectRequest(
                EffectType.NETWORK,
                "http-get",
                [f"https://example.com/api/{i}"]
            )
            self.handler.handle(request)
        
        # Get history
        history_req = EffectRequest(
            EffectType.NETWORK,
            "get-request-history",
            []
        )
        history_result = self.handler.handle(history_req)
        
        self.assertEqual(len(history_result.value), 5)
        
        # Get limited history
        limited_req = EffectRequest(
            EffectType.NETWORK,
            "get-request-history",
            [3]
        )
        limited_result = self.handler.handle(limited_req)
        
        self.assertEqual(len(limited_result.value), 3)
        
        # Clear history
        clear_req = EffectRequest(
            EffectType.NETWORK,
            "clear-request-history",
            []
        )
        self.handler.handle(clear_req)
        
        # Verify cleared
        history_result = self.handler.handle(history_req)
        self.assertEqual(len(history_result.value), 0)
    
    def test_host_restrictions(self):
        """Test host restriction functionality"""
        # Create handler with restrictions
        restricted_handler = NetworkHandler(
            allow_external=True,
            allowed_hosts=["example.com", "*.safe.com"],
            mock_mode=True
        )
        
        # Allowed host
        allowed_req = EffectRequest(
            EffectType.NETWORK,
            "http-get",
            ["https://example.com/api"]
        )
        allowed_result = restricted_handler.handle(allowed_req)
        self.assertEqual(allowed_result.value["status"], 200)
        
        # Disallowed host
        disallowed_req = EffectRequest(
            EffectType.NETWORK,
            "http-get",
            ["https://evil.com/api"]
        )
        disallowed_result = restricted_handler.handle(disallowed_req)
        
        # Should have error effect
        self.assertIsNone(disallowed_result.value)
        self.assertTrue(len(disallowed_result.secondary_effects) > 0)
        error_effect = disallowed_result.secondary_effects[0]
        self.assertEqual(error_effect.effect_type, EffectType.ERROR)
    
    def test_rate_limiting(self):
        """Test rate limiting functionality"""
        # Make many requests quickly
        for i in range(105):
            request = EffectRequest(
                EffectType.NETWORK,
                "http-get",
                [f"https://example.com/api"]
            )
            result = self.handler.handle(request)
            
            if i < 100:
                # Should succeed
                self.assertEqual(result.value["status"], 200)
            else:
                # Should be rate limited
                self.assertIsNone(result.value)
                self.assertTrue(len(result.secondary_effects) > 0)
    
    def test_unknown_operation(self):
        """Test handling of unknown operation"""
        request = EffectRequest(
            EffectType.NETWORK,
            "unknown-op",
            []
        )
        
        with self.assertRaises(ValueError) as cm:
            self.handler.handle(request)
        
        self.assertIn("Unknown network operation", str(cm.exception))


class TestHttpRequest(unittest.TestCase):
    """Test HTTP request data structure"""
    
    def test_request_creation(self):
        """Test creating HTTP request"""
        req = HttpRequest(
            method="POST",
            url="https://api.example.com/users",
            headers={"Content-Type": "application/json"},
            json_data={"name": "Alice"}
        )
        
        self.assertEqual(req.method, "POST")
        self.assertEqual(req.url, "https://api.example.com/users")
        self.assertEqual(req.headers["Content-Type"], "application/json")
        self.assertEqual(req.json_data["name"], "Alice")
        self.assertEqual(req.timeout, 30.0)
        self.assertTrue(req.follow_redirects)


class TestWebSocketMessage(unittest.TestCase):
    """Test WebSocket message structure"""
    
    def test_text_message(self):
        """Test text WebSocket message"""
        msg = WebSocketMessage(type="text", data="Hello, WebSocket!")
        
        self.assertEqual(msg.type, "text")
        self.assertEqual(msg.data, "Hello, WebSocket!")
        self.assertIsInstance(msg.timestamp, float)
    
    def test_binary_message(self):
        """Test binary WebSocket message"""
        data = b"\x00\x01\x02\x03"
        msg = WebSocketMessage(type="binary", data=data)
        
        self.assertEqual(msg.type, "binary")
        self.assertEqual(msg.data, data)


if __name__ == "__main__":
    unittest.main()