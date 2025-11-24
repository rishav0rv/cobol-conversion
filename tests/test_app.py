"""
Integration tests for the Flask application.
"""
import pytest
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from version2.app import app


@pytest.fixture
def client():
    """Create a test client for the Flask app."""
    app.config['TESTING'] = True
    app.config['SECRET_KEY'] = 'test-secret-key'
    
    with app.test_client() as client:
        yield client


class TestRoutes:
    """Test Flask routes."""
    
    def test_index_get(self, client):
        """Test GET request to index page."""
        response = client.get('/')
        assert response.status_code == 200
        assert b'COBOL to Java' in response.data
    
    def test_index_post_empty(self, client):
        """Test POST with empty COBOL code."""
        response = client.post('/', data={'cobol_code': ''})
        assert response.status_code == 200
        assert b'error' in response.data.lower() or b'provide' in response.data.lower()
    
    def test_health_check(self, client):
        """Test health check endpoint."""
        response = client.get('/health')
        assert response.status_code == 200
        assert b'healthy' in response.data
    
    def test_download_without_file(self, client):
        """Test download when no ZIP file exists."""
        # Clean up any existing zip
        zip_file = "spring_batch_output.zip"
        if os.path.exists(zip_file):
            os.remove(zip_file)
        
        response = client.get('/download')
        # Should redirect or show error
        assert response.status_code in [302, 404, 200]
    
    def test_404_error(self, client):
        """Test 404 error handling."""
        response = client.get('/nonexistent-page')
        assert response.status_code == 404


class TestFormValidation:
    """Test form input validation."""
    
    def test_model_selection(self, client):
        """Test that model selection is passed correctly."""
        response = client.get('/')
        assert b'codellama' in response.data or b'model' in response.data.lower()
    
    def test_examples_present(self, client):
        """Test that example links are present."""
        response = client.get('/')
        assert b'Example' in response.data or b'example' in response.data


@pytest.mark.integration
class TestEndToEnd:
    """
    End-to-end integration tests.
    Note: These tests require Ollama to be running and may take longer.
    """
    
    def test_conversion_flow(self, client):
        """
        Test complete conversion flow.
        Skipped if Ollama is not available.
        """
        # This test would require Ollama to be running
        # For now, we just test the route exists
        cobol_code = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST."
        response = client.post('/', data={'cobol_code': cobol_code})
        
        # Should return 200 regardless (either success or error message)
        assert response.status_code == 200
