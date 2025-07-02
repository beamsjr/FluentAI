"""
Input validation and sanitization

Provides validators for common security-sensitive inputs.
"""

import os
import re
from pathlib import Path
from urllib.parse import urlparse, quote
from typing import Optional, List, Pattern, Union


class ValidationError(ValueError):
    """Raised when validation fails"""
    pass


class PathValidator:
    """Validates and sanitizes file paths"""
    
    def __init__(self, 
                 allowed_dirs: Optional[List[str]] = None,
                 disallow_symlinks: bool = True,
                 disallow_absolute: bool = False):
        self.allowed_dirs = [Path(d).resolve() for d in (allowed_dirs or [])]
        self.disallow_symlinks = disallow_symlinks
        self.disallow_absolute = disallow_absolute
    
    def validate(self, path: Union[str, Path]) -> Path:
        """Validate a file path"""
        path = Path(path)
        
        # Check for path traversal attempts
        if '..' in path.parts:
            raise ValidationError(f"Path traversal detected: {path}")
        
        # Check absolute paths
        if self.disallow_absolute and path.is_absolute():
            raise ValidationError(f"Absolute paths not allowed: {path}")
        
        # Resolve the path (follows symlinks)
        try:
            resolved_path = path.resolve()
        except (OSError, RuntimeError) as e:
            raise ValidationError(f"Invalid path: {path}") from e
        
        # Check symlinks
        if self.disallow_symlinks and path.exists() and path.is_symlink():
            raise ValidationError(f"Symlinks not allowed: {path}")
        
        # Check allowed directories
        if self.allowed_dirs:
            allowed = False
            for allowed_dir in self.allowed_dirs:
                try:
                    resolved_path.relative_to(allowed_dir)
                    allowed = True
                    break
                except ValueError:
                    continue
            
            if not allowed:
                raise ValidationError(
                    f"Path {resolved_path} not in allowed directories"
                )
        
        return resolved_path
    
    def sanitize(self, path: Union[str, Path]) -> str:
        """Sanitize a path for safe use"""
        # Remove any null bytes
        if isinstance(path, str):
            path = path.replace('\0', '')
        
        # Normalize the path
        path = Path(path)
        
        # Remove any special characters that might cause issues
        parts = []
        for part in path.parts:
            # Keep only safe characters
            safe_part = re.sub(r'[^\w\-_\. ]', '', part)
            if safe_part and safe_part not in ('.', '..'):
                parts.append(safe_part)
        
        return os.path.join(*parts) if parts else ''


class URLValidator:
    """Validates and sanitizes URLs"""
    
    def __init__(self,
                 allowed_schemes: Optional[List[str]] = None,
                 allowed_hosts: Optional[List[str]] = None,
                 disallow_credentials: bool = True):
        self.allowed_schemes = allowed_schemes or ['http', 'https']
        self.allowed_hosts = allowed_hosts
        self.disallow_credentials = disallow_credentials
    
    def validate(self, url: str) -> str:
        """Validate a URL"""
        if not url:
            raise ValidationError("Empty URL")
        
        # Parse the URL
        try:
            parsed = urlparse(url)
        except Exception as e:
            raise ValidationError(f"Invalid URL: {url}") from e
        
        # Check scheme
        if parsed.scheme not in self.allowed_schemes:
            raise ValidationError(
                f"URL scheme {parsed.scheme} not allowed. "
                f"Allowed: {self.allowed_schemes}"
            )
        
        # Check for credentials
        if self.disallow_credentials and (parsed.username or parsed.password):
            raise ValidationError("URLs with credentials not allowed")
        
        # Check host
        if self.allowed_hosts and parsed.hostname:
            host_allowed = False
            for allowed_host in self.allowed_hosts:
                if allowed_host.startswith('*.'):
                    # Wildcard domain
                    suffix = allowed_host[2:]
                    if parsed.hostname.endswith(suffix):
                        host_allowed = True
                        break
                elif parsed.hostname == allowed_host:
                    host_allowed = True
                    break
            
            if not host_allowed:
                raise ValidationError(
                    f"Host {parsed.hostname} not in allowed hosts"
                )
        
        return url
    
    def sanitize(self, url: str) -> str:
        """Sanitize a URL"""
        # Remove any null bytes
        url = url.replace('\0', '')
        
        # Parse and reconstruct to ensure proper encoding
        try:
            parsed = urlparse(url)
            
            # Ensure path is properly quoted
            if parsed.path:
                path = quote(parsed.path, safe='/')
            else:
                path = parsed.path
            
            # Reconstruct URL without credentials
            if self.disallow_credentials:
                netloc = parsed.hostname or ''
                if parsed.port:
                    netloc += f':{parsed.port}'
            else:
                netloc = parsed.netloc
            
            return parsed._replace(
                netloc=netloc,
                path=path
            ).geturl()
            
        except Exception:
            # If parsing fails, return empty string
            return ''


class InputSanitizer:
    """General input sanitization"""
    
    # Patterns for common injections
    SQL_INJECTION_PATTERN = re.compile(
        r'(\b(SELECT|INSERT|UPDATE|DELETE|DROP|UNION|ALTER|CREATE)\b)|'
        r'(--|;|\'|"|\||\\)',
        re.IGNORECASE
    )
    
    XSS_PATTERN = re.compile(
        r'(<script|javascript:|onerror=|onload=|onclick=|<iframe|<object|<embed)',
        re.IGNORECASE
    )
    
    COMMAND_INJECTION_PATTERN = re.compile(
        r'([;&|`$]|\$\(|\${|<\(|>\()'
    )
    
    @staticmethod
    def sanitize_sql(value: str) -> str:
        """Sanitize input for SQL contexts"""
        # Remove or escape dangerous SQL patterns
        return InputSanitizer.SQL_INJECTION_PATTERN.sub('', value)
    
    @staticmethod
    def sanitize_html(value: str) -> str:
        """Sanitize input for HTML contexts"""
        # Escape HTML special characters
        # Note: & must be replaced first to avoid double-escaping
        replacements = [
            ('&', '&amp;'),
            ('<', '&lt;'),
            ('>', '&gt;'),
            ('"', '&quot;'),
            ("'", '&#x27;')
        ]
        
        for char, escaped in replacements:
            value = value.replace(char, escaped)
        
        return value
    
    @staticmethod
    def sanitize_shell(value: str) -> str:
        """Sanitize input for shell contexts"""
        # Remove shell metacharacters
        return InputSanitizer.COMMAND_INJECTION_PATTERN.sub('', value)
    
    @staticmethod
    def sanitize_filename(filename: str) -> str:
        """Sanitize a filename"""
        # Remove path separators and null bytes
        filename = filename.replace('/', '').replace('\\', '').replace('\0', '')
        
        # Remove leading dots (hidden files)
        filename = filename.lstrip('.')
        
        # Keep only safe characters
        filename = re.sub(r'[^\w\-_\. ]', '', filename)
        
        # Limit length
        max_length = 255
        if len(filename) > max_length:
            name, ext = os.path.splitext(filename)
            name = name[:max_length - len(ext)]
            filename = name + ext
        
        return filename or 'unnamed'


# Convenience functions
def validate_path(path: Union[str, Path], 
                  allowed_dirs: Optional[List[str]] = None) -> Path:
    """Validate a file path"""
    validator = PathValidator(allowed_dirs=allowed_dirs)
    return validator.validate(path)


def validate_url(url: str,
                 allowed_hosts: Optional[List[str]] = None) -> str:
    """Validate a URL"""
    validator = URLValidator(allowed_hosts=allowed_hosts)
    return validator.validate(url)


def validate_host(host: str, allowed_hosts: List[str]) -> bool:
    """Check if a host is in the allowed list"""
    for allowed_host in allowed_hosts:
        if allowed_host.startswith('*.'):
            # Wildcard domain
            suffix = allowed_host[2:]
            if host.endswith(suffix):
                return True
        elif host == allowed_host:
            return True
    
    return False


def sanitize_input(value: str, 
                   context: str = 'general') -> str:
    """Sanitize input based on context"""
    if context == 'sql':
        return InputSanitizer.sanitize_sql(value)
    elif context == 'html':
        return InputSanitizer.sanitize_html(value)
    elif context == 'shell':
        return InputSanitizer.sanitize_shell(value)
    elif context == 'filename':
        return InputSanitizer.sanitize_filename(value)
    else:
        # General sanitization - remove null bytes and control characters
        return ''.join(char for char in value 
                      if ord(char) >= 32 or char in '\n\r\t')