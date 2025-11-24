"""
Flask web application for COBOL to Java Spring Batch conversion.
"""
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from flask import Flask, request, render_template, send_file, flash, redirect, url_for, jsonify
from version2.generator import call_ollama, OllamaConnectionError, OllamaTimeoutError
from version2.file_writer import (extract_files_from_response, write_files_to_project, 
                         write_pom, write_readme, zip_project, cleanup_old_output)
from config import config
from utils.logger import setup_logger


# Initialize Flask app
app = Flask(__name__)

# Load configuration
cfg = config['default']
app.config.from_object(cfg)
app.secret_key = cfg.SECRET_KEY

# Setup logging
logger = setup_logger('app')

@app.route("/", methods=["GET", "POST"])
def index():
    """Main conversion page."""
    java_files = {}
    cobol_code = ""
    zip_path = ""
    error_message = ""
    
    if request.method == "POST":
        try:
            cobol_code = request.form.get("cobol_code", "").strip()
            selected_model = request.form.get("model", cfg.OLLAMA_MODEL)
            
            if not cobol_code:
                error_message = "Please provide COBOL code to convert"
                logger.warning("Empty COBOL code submitted")
            else:
                logger.info(f"Received conversion request ({len(cobol_code)} chars)")
                
                # Clean up old output
                cleanup_old_output("output_project")
                
                # Call Ollama to generate code
                try:
                    response = call_ollama(cobol_code, model=selected_model)
                    
                    # Extract Java files from response
                    java_files = extract_files_from_response(response)
                    
                    # Write files to project
                    write_files_to_project(java_files)
                    
                    # Write pom.xml and README
                    write_pom()
                    write_readme()
                    
                    # Create ZIP
                    zip_path = zip_project()
                    
                    logger.info("Conversion completed successfully")
                    flash("Conversion completed successfully!", "success")
                    
                except ValueError as e:
                    error_message = str(e)
                    logger.error(f"Validation error: {e}")
                    
                except OllamaConnectionError as e:
                    error_message = (
                        "Cannot connect to Ollama service. "
                        "Please ensure Ollama is running (ollama serve) and try again. "
                        f"Details: {str(e)}"
                    )
                    logger.error(f"Ollama connection error: {e}")
                    
                except OllamaTimeoutError as e:
                    error_message = (
                        "The conversion request timed out. "
                        "Try with shorter COBOL code or check if Ollama is responding. "
                        f"Details: {str(e)}"
                    )
                    logger.error(f"Timeout error: {e}")
                    
                except Exception as e:
                    error_message = f"An unexpected error occurred: {str(e)}"
                    logger.exception("Unexpected error during conversion")
                    
        except Exception as e:
            error_message = f"Server error: {str(e)}"
            logger.exception("Server error handling request")
    
    return render_template(
        "index.html",
        java_files=java_files,
        cobol_code=cobol_code,
        zip_path=zip_path,
        error_message=error_message,
        available_models=['codellama', 'deepseek-coder', 'codellama:13b']
    )

@app.route("/download")
def download_zip():
    """Download the generated Spring Batch project ZIP."""
    zip_file = "spring_batch_output.zip"
    
    if not os.path.exists(zip_file):
        logger.error(f"ZIP file not found: {zip_file}")
        flash("Download file not found. Please generate the project first.", "error")
        return redirect(url_for('index'))
    
    try:
        logger.info(f"Serving download: {zip_file}")
        return send_file(
            zip_file,
            as_attachment=True,
            download_name="spring_batch_project.zip",
            mimetype='application/zip'
        )
    except Exception as e:
        logger.error(f"Error serving download: {e}")
        flash(f"Error downloading file: {str(e)}", "error")
        return redirect(url_for('index'))

@app.route("/health")
def health_check():
    """Health check endpoint."""
    return jsonify({
        "status": "healthy",
        "service": "COBOL to Java Converter"
    })

@app.errorhandler(404)
def not_found(error):
    """Handle 404 errors."""
    return render_template('index.html', error_message="Page not found"), 404

@app.errorhandler(500)
def internal_error(error):
    """Handle 500 errors."""
    logger.error(f"Internal server error: {error}")
    return render_template('index.html', error_message="Internal server error"), 500

if __name__ == "__main__":
    app.run(debug=cfg.DEBUG, host='0.0.0.0', port=5000)

