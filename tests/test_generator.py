"""
Tests for the generator module.
"""
import pytest
from unittest.mock import patch, Mock
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from version2.generator import (
    call_ollama, 
    validate_cobol_input, 
    check_ollama_health,
    OllamaConnectionError,
    OllamaTimeoutError
)


class TestValidation:
    """Test input validation."""
    
    def test_empty_cobol_code(self):
        """Test that empty COBOL code is rejected."""
        is_valid, error = validate_cobol_input("")
        assert not is_valid
        assert "empty" in error.lower()
    
    def test_whitespace_only_cobol(self):
        """Test that whitespace-only input is rejected."""
        is_valid, error = validate_cobol_input("   \n\n   ")
        assert not is_valid
        assert "empty" in error.lower()
    
    def test_too_long_cobol(self):
        """Test that excessively long input is rejected."""
        long_code = "A" * 200000  # Exceeds default limit
        is_valid, error = validate_cobol_input(long_code)
        assert not is_valid
        assert "maximum length" in error.lower()
    
    def test_valid_cobol(self):
        """Test that valid COBOL code passes validation."""
        valid_code = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST."
        is_valid, error = validate_cobol_input(valid_code)
        assert is_valid
        assert error is None


class TestOllamaHealth:
    """Test Ollama health checks."""
    
    @patch('version2.generator.requests.get')
    def test_ollama_available(self, mock_get):
        """Test when Ollama service is available."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_get.return_value = mock_response
        
        assert check_ollama_health() is True
    
    @patch('version2.generator.requests.get')
    def test_ollama_unavailable_bad_status(self, mock_get):
        """Test when Ollama returns non-200 status."""
        mock_response = Mock()
        mock_response.status_code = 500
        mock_get.return_value = mock_response
        
        with pytest.raises(OllamaConnectionError):
            check_ollama_health()
    
    @patch('version2.generator.requests.get')
    def test_ollama_connection_error(self, mock_get):
        """Test when cannot connect to Ollama."""
        mock_get.side_effect = Exception("Connection refused")
        
        with pytest.raises(OllamaConnectionError):
            check_ollama_health()


class TestCallOllama:
    """Test Ollama API calls."""
    
    @patch('version2.generator.requests.get')
    @patch('version2.generator.requests.post')
    def test_successful_conversion(self, mock_post, mock_get):
        """Test successful COBOL to Java conversion."""
        # Mock health check
        mock_health = Mock()
        mock_health.status_code = 200
        mock_get.return_value = mock_health
        
        # Mock Ollama response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "response": "```java filename: Test.java\npublic class Test {}\n```"
        }
        mock_post.return_value = mock_response
        
        result = call_ollama("IDENTIFICATION DIVISION.")
        assert "java" in result.lower()
        assert len(result) > 0
    
    @patch('version2.generator.requests.get')
    def test_empty_cobol_raises_error(self, mock_get):
        """Test that empty COBOL raises ValueError."""
        with pytest.raises(ValueError, match="empty"):
            call_ollama("")
    
    @patch('version2.generator.requests.get')
    @patch('version2.generator.requests.post')
    def test_ollama_timeout(self, mock_post, mock_get):
        """Test timeout handling."""
        # Mock health check
        mock_health = Mock()
        mock_health.status_code = 200
        mock_get.return_value = mock_health
        
        # Mock timeout
        import requests
        mock_post.side_effect = requests.exceptions.Timeout()
        
        with pytest.raises(OllamaTimeoutError):
            call_ollama("IDENTIFICATION DIVISION.")
    
    @patch('version2.generator.requests.get')
    @patch('version2.generator.requests.post')
    def test_empty_response(self, mock_post, mock_get):
        """Test handling of empty response from Ollama."""
        # Mock health check
        mock_health = Mock()
        mock_health.status_code = 200
        mock_get.return_value = mock_health
        
        # Mock empty response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": ""}
        mock_post.return_value = mock_response
        
        with pytest.raises(ValueError, match="empty response"):
            call_ollama("IDENTIFICATION DIVISION.")
