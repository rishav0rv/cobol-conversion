"""
Centralized configuration for the COBOL to Java converter application.
"""
import os

class Config:
    """Application configuration."""
    
    # Flask settings
    DEBUG = os.getenv('FLASK_DEBUG', 'True').lower() == 'true'
    SECRET_KEY = os.getenv('SECRET_KEY', 'dev-secret-key-change-in-production')
    
    # Ollama settings
    OLLAMA_BASE_URL = os.getenv('OLLAMA_URL', 'http://localhost:11434')
    OLLAMA_MODEL = os.getenv('OLLAMA_MODEL', 'codellama')
    OLLAMA_TIMEOUT = int(os.getenv('OLLAMA_TIMEOUT', '120'))  # seconds
    OLLAMA_MAX_RETRIES = int(os.getenv('OLLAMA_MAX_RETRIES', '3'))
    
    # Output settings
    OUTPUT_DIR = os.getenv('OUTPUT_DIR', 'version2/output_project')
    ZIP_NAME = os.getenv('ZIP_NAME', 'spring_batch_output')
    
    # Upload limits
    MAX_COBOL_LENGTH = int(os.getenv('MAX_COBOL_LENGTH', '100000'))  # characters
    
    # Logging
    LOG_LEVEL = os.getenv('LOG_LEVEL', 'INFO')
    LOG_FILE = os.getenv('LOG_FILE', 'logs/converter.log')

class ProductionConfig(Config):
    """Production configuration."""
    DEBUG = False
    
class DevelopmentConfig(Config):
    """Development configuration."""
    DEBUG = True

class TestConfig(Config):
    """Test configuration."""
    DEBUG = True
    TESTING = True
    OLLAMA_TIMEOUT = 5
    
# Configuration dictionary
config = {
    'development': DevelopmentConfig,
    'production': ProductionConfig,
    'test': TestConfig,
    'default': DevelopmentConfig
}
