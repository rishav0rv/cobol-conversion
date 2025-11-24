# 🔄 COBOL to Java Spring Batch Converter

Transform legacy COBOL code into modern Java Spring Batch applications using AI-powered code generation.

## ✨ Features

- **AI-Powered Conversion**: Leverages Ollama LLM (CodeLlama, DeepSeek-Coder) for intelligent code transformation
- **Complete Project Generation**: Creates full Maven Spring Batch projects with proper structure
- **Modern Web Interface**: Beautiful, responsive UI with real-time feedback
- **Multiple File Support**: Automatically extracts and organizes multiple Java classes
- **Robust Error Handling**: Comprehensive validation, retry logic, and user-friendly error messages
- **Downloadable Projects**: Get complete ZIP archives ready to build and run
- **Example Templates**: Quick-start with sample COBOL code

## 📋 Prerequisites

### Required
- **Python 3.8+**
- **Ollama**: Local LLM server
  ```bash
  # Install Ollama (https://ollama.ai)
  # Download and install from https://ollama.ai/download
  
  # Pull the CodeLlama model
  ollama pull codellama
  
  # Start Ollama service
  ollama serve
  ```

### Optional
- **Java 11+** and **Maven** 3.6+ (to build generated projects)

## 🚀 Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/yourusername/cobol-to-java-converter.git
   cd cobol-to-java-converter
   ```

2. **Install Python dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Ensure Ollama is running**
   ```bash
   ollama serve
   ```

## 💻 Usage

### Start the Application

```bash
python run.py
```

The application will be available at `http://localhost:5000`

### Using the Web Interface

1.  **Paste COBOL Code** into the textarea
   - Or click on example templates to load sample code

2. **Select AI Model** (optional)
   - Default: `codellama`
   - Alternatives: `deepseek-coder`, `codellama:13b`

3. **Click "Convert to Java Spring Batch"**
   - Wait for the AI to generate your Java code
   - View generated files in the browser

4. **Download ZIP**
   - Click "Download Complete Spring Batch Project"
   - Extract and build with Maven

### Building Generated Projects

```bash
# Extract the downloaded ZIP
unzip spring_batch_project.zip
cd output_project

# Build with Maven
mvn clean install

# Run (configure input/output paths first)
mvn spring-boot:run
```

## 🏗️ Project Structure

```
.
├── config.py                    # Configuration management
├── run.py                       # Main entry point
├── requirements.txt             # Python dependencies
├── utils/
│   └── logger.py               # Logging configuration
├── version2/                    # Main application
│   ├── app.py                  # Flask web application
│   ├── generator.py            # Ollama integration
│   ├── file_writer.py          # File extraction & project generation
│   ├── static/                 # CSS and JavaScript
│   │   ├── styles.css
│   │   └── script.js
│   └── templates/              # HTML templates
│       └── index.html
└── tests/                       # Test suite
    ├── test_generator.py
    ├── test_file_writer.py
    ├── test_app.py
    └── fixtures/
        └── sample_cobol.txt
```

## ⚙️ Configuration

Configuration can be customized via environment variables or `config.py`:

```bash
# Ollama settings
export OLLAMA_URL="http://localhost:11434"
export OLLAMA_MODEL="codellama"
export OLLAMA_TIMEOUT="120"

# Application settings
export FLASK_DEBUG="True"
export MAX_COBOL_LENGTH="100000"
```

## 🧪 Testing

Run the test suite:

```bash
# Run all tests with coverage
pytest tests/ -v --cov=version2

# Run specific test modules
pytest tests/test_generator.py -v
pytest tests/test_file_writer.py -v
pytest tests/test_app.py -v

# Run without integration tests (Ollama not required)
pytest tests/ -v -m "not integration"
```

## 🎨 Features in Detail

### Error Handling
- Input validation (empty code, length limits)
- Ollama connection errors with helpful messages
- Timeout handling with automatic retries
- Graceful degradation when service unavailable

### Logging
- Comprehensive logging to file and console
- Rotating log files (10MB max, 5 backups)
- Configurable log levels
- Located in `logs/converter.log`

### UI/UX
- Modern gradient design
- Loading spinner during conversion
- Copy-to-clipboard for generated code
- Responsive mobile layout
- Sample COBOL examples
- Character counter
- Error and success alerts

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🐛 Troubleshooting

### "Cannot connect to Ollama service"
- Ensure Ollama is installed and running: `ollama serve`
- Check the URL configuration matches your Ollama instance
- Verify the model is available: `ollama list`

### "Request timed out"
- Try with shorter COBOL code
- Increase timeout in config: `export OLLAMA_TIMEOUT="180"`
- Check Ollama service health

### "No valid Java files found"
- The AI model may need better prompting
- Try a different model (deepseek-coder may work better for some cases)
- Check that your COBOL code is well-formatted

### ZIP Download Not Working
- Ensure conversion completed successfully
- Check logs for file generation errors
- Verify output_project directory has files

## 🔗 Links

- [Ollama](https://ollama.ai) - Local LLM runtime
- [Spring Batch](https://spring.io/projects/spring-batch) - Batch processing framework
- [Flask](https://flask.palletsprojects.com/) - Python web framework

## 📧 Support

For issues, questions, or suggestions, please open an issue on GitHub.

---

**Made with ❤️ for Legacy System Modernization**
