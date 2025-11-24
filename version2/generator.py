"""
COBOL to Java Spring Batch code generation using Ollama LLM.
"""
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import requests
import time
from config import config
from utils.logger import setup_logger

logger = setup_logger('generator')
cfg = config['default']

PROMPT_TEMPLATE = """
You are a COBOL-to-Java Spring Batch converter.

Convert the following COBOL code into a working Java Spring Batch project. Break your output into separate files as follows:

- BatchConfig.java
- InRecord.java (based on FILE SECTION input structure)
- OutRecord.java (based on output file format)
- RecordProcessor.java (business logic from PROCEDURE DIVISION)

Use the format:
```java filename: BatchConfig.java
// your code
```
```java filename: InRecord.java
// your code
```
and so on.

COBOL CODE:
{cobol_code}
"""

class OllamaConnectionError(Exception):
    """Raised when cannot connect to Ollama service."""
    pass

class OllamaTimeoutError(Exception):
    """Raised when Ollama request times out."""
    pass

def validate_cobol_input(cobol_code):
    """
    Validate COBOL code input.
    
    Args:
        cobol_code: COBOL code string
        
    Returns:
        tuple: (is_valid, error_message)
    """
    if not cobol_code or not cobol_code.strip():
        return False, "COBOL code cannot be empty"
    
    if len(cobol_code) > cfg.MAX_COBOL_LENGTH:
        return False, f"COBOL code exceeds maximum length of {cfg.MAX_COBOL_LENGTH} characters"
    
    return True, None

def check_ollama_health():
    """
    Check if Ollama service is available.
    
    Returns:
        bool: True if service is healthy
        
    Raises:
        OllamaConnectionError: If service is not available
    """
    try:
        response = requests.get(f"{cfg.OLLAMA_BASE_URL}/api/tags", timeout=5)
        if response.status_code == 200:
            logger.info("Ollama service is available")
            return True
        else:
            raise OllamaConnectionError(f"Ollama service returned status {response.status_code}")
    except requests.exceptions.RequestException as e:
        raise OllamaConnectionError(f"Cannot connect to Ollama at {cfg.OLLAMA_BASE_URL}: {str(e)}")

def call_ollama(cobol_code, model=None):
    """
    Call Ollama API to convert COBOL to Java Spring Batch code.
    
    Args:
        cobol_code: COBOL code to convert
        model: Ollama model to use (defaults to config value)
        
    Returns:
        str: Generated Java code
        
    Raises:
        OllamaConnectionError: If cannot connect to Ollama
        OllamaTimeoutError: If request times out
        ValueError: If input validation fails
    """
    # Validate input
    is_valid, error_msg = validate_cobol_input(cobol_code)
    if not is_valid:
        logger.error(f"Invalid COBOL input: {error_msg}")
        raise ValueError(error_msg)
    
    # Check Ollama health
    try:
        check_ollama_health()
    except OllamaConnectionError as e:
        logger.error(f"Ollama health check failed: {e}")
        raise
    
    model = model or cfg.OLLAMA_MODEL
    prompt = PROMPT_TEMPLATE.format(cobol_code=cobol_code)
    
    logger.info(f"Starting COBOL to Java conversion using model: {model}")
    logger.info(f"COBOL code length: {len(cobol_code)} characters")
    
    # Retry logic
    for attempt in range(cfg.OLLAMA_MAX_RETRIES):
        try:
            logger.info(f"Attempt {attempt + 1}/{cfg.OLLAMA_MAX_RETRIES}")
            
            response = requests.post(
                f"{cfg.OLLAMA_BASE_URL}/api/generate",
                json={
                    "model": model,
                    "prompt": prompt,
                    "stream": False
                },
                timeout=cfg.OLLAMA_TIMEOUT
            )
            
            if response.status_code == 200:
                result = response.json()
                generated_code = result.get("response", "")
                
                if not generated_code:
                    logger.warning("Ollama returned empty response")
                    if attempt < cfg.OLLAMA_MAX_RETRIES - 1:
                        logger.info("Retrying...")
                        time.sleep(2)
                        continue
                    else:
                        raise ValueError("Ollama returned empty response after all retries")
                
                logger.info(f"Successfully generated {len(generated_code)} characters of Java code")
                return generated_code
            else:
                error_msg = f"Ollama API returned status {response.status_code}: {response.text}"
                logger.error(error_msg)
                
                if attempt < cfg.OLLAMA_MAX_RETRIES - 1:
                    logger.info(f"Retrying in 2 seconds...")
                    time.sleep(2)
                else:
                    raise OllamaConnectionError(error_msg)
                    
        except requests.exceptions.Timeout:
            logger.warning(f"Request timed out after {cfg.OLLAMA_TIMEOUT} seconds")
            if attempt < cfg.OLLAMA_MAX_RETRIES - 1:
                logger.info("Retrying...")
                time.sleep(2)
            else:
                raise OllamaTimeoutError(
                    f"Request timed out after {cfg.OLLAMA_MAX_RETRIES} attempts. "
                    "Try with shorter COBOL code or increase timeout in config."
                )
                
        except requests.exceptions.RequestException as e:
            logger.error(f"Request failed: {e}")
            if attempt < cfg.OLLAMA_MAX_RETRIES - 1:
                logger.info("Retrying...")
                time.sleep(2)
            else:
                raise OllamaConnectionError(f"Failed to connect to Ollama: {str(e)}")
    
    # Should not reach here, but just in case
    raise OllamaConnectionError("Failed to generate code after all retries")

