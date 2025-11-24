"""
Main entry point for the COBOL to Java Spring Batch converter.
"""
import os
import sys
import requests
from config import config
from utils.logger import setup_logger

# Set up logging
logger = setup_logger()

def check_ollama_availability():
    """Check if Ollama service is running and accessible."""
    cfg = config['default']
    try:
        logger.info(f"Checking Ollama service at {cfg.OLLAMA_BASE_URL}...")
        response = requests.get(f"{cfg.OLLAMA_BASE_URL}/api/tags", timeout=5)
        if response.status_code == 200:
            logger.info("✓ Ollama service is running")
            return True
        else:
            logger.warning(f"⚠ Ollama service returned status code: {response.status_code}")
            return False
    except requests.exceptions.RequestException as e:
        logger.warning(f"⚠ Cannot connect to Ollama service: {e}")
        logger.warning(f"  The application will start, but conversions will fail.")
        logger.warning(f"  Please ensure Ollama is running: ollama serve")
        return False

def main():
    """Main entry point."""
    logger.info("="*60)
    logger.info("COBOL to Java Spring Batch Converter")
    logger.info("="*60)
    
    # Pre-flight checks
    check_ollama_availability()
    
    # Import and run the Flask app
    # We import here to avoid circular imports
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'version2'))
    from version2.app import app
    
    cfg = config['default']
    logger.info(f"Starting Flask application on http://localhost:5000")
    logger.info(f"Debug mode: {cfg.DEBUG}")
    logger.info("Press CTRL+C to stop the server")
    logger.info("="*60)
    
    try:
        app.run(debug=cfg.DEBUG, host='0.0.0.0', port=5000)
    except KeyboardInterrupt:
        logger.info("\nShutting down gracefully...")
    except Exception as e:
        logger.error(f"Error starting application: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()
