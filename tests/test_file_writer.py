"""
Tests for the file_writer module.
"""
import pytest
import os
import sys
import tempfile
import shutil

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from version2.file_writer import (
    extract_files_from_response,
    write_files_to_project,
    write_pom,
    write_readme,
    zip_project
)


class TestFileExtraction:
    """Test Java file extraction from Ollama response."""
    
    def test_extract_single_file(self):
        """Test extracting a single Java file."""
        response = """
Here is the code:
```java filename: Test.java
public class Test {
    public static void main(String[] args) {
        System.out.println("Hello");
    }
}
```
"""
        files = extract_files_from_response(response)
        assert len(files) == 1
        assert "Test.java" in files
        assert "public class Test" in files["Test.java"]
    
    def test_extract_multiple_files(self):
        """Test extracting multiple Java files."""
        response = """
```java filename: Config.java
public class Config {}
```

```java filename: Processor.java
public class Processor {}
```
"""
        files = extract_files_from_response(response)
        assert len(files) == 2
        assert "Config.java" in files
        assert "Processor.java" in files
    
    def test_no_files_raises_error(self):
        """Test that response with no code blocks raises error."""
        response = "This is just text with no code blocks."
        
        with pytest.raises(ValueError, match="No valid Java files"):
            extract_files_from_response(response)
    
    def test_fallback_extraction(self):
        """Test fallback extraction for code without filename."""
        response = """
```java
public class Unnamed {
    // Some code
}
```
"""
        files = extract_files_from_response(response)
        assert len(files) >= 1
        # Should create GeneratedCode1.java or similar


class TestFileWriting:
    """Test writing Java files to disk."""
    
    def setup_method(self):
        """Create a temporary directory for each test."""
        self.temp_dir = tempfile.mkdtemp()
    
    def teardown_method(self):
        """Clean up temporary directory."""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
    
    def test_write_files(self):
        """Test writing files to project directory."""
        files = {
            "Test.java": "public class Test {}",
            "Config.java": "public class Config {}"
        }
        
        base_dir = os.path.join(self.temp_dir, "src/main/java")
        count = write_files_to_project(files, base_dir=base_dir)
        
        assert count == 2
        assert os.path.exists(os.path.join(base_dir, "Test.java"))
        assert os.path.exists(os.path.join(base_dir, "Config.java"))
    
    def test_write_pom(self):
        """Test pom.xml generation."""
        write_pom(output_dir=self.temp_dir)
        
        pom_path = os.path.join(self.temp_dir, "pom.xml")
        assert os.path.exists(pom_path)
        
        with open(pom_path, 'r') as f:
            content = f.read()
            assert "spring-boot-starter-batch" in content
            assert "maven" in content.lower()
    
    def test_write_readme(self):
        """Test README generation."""
        write_readme(output_dir=self.temp_dir)
        
        readme_path = os.path.join(self.temp_dir, "README.md")
        assert os.path.exists(readme_path)
        
        with open(readme_path, 'r') as f:
            content = f.read()
            assert "Spring Batch" in content
            assert "mvn" in content


class TestZipCreation:
    """Test ZIP archive creation."""
    
    def setup_method(self):
        """Create a temporary directory with some files."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_file = os.path.join(self.temp_dir, "test.txt")
        with open(self.test_file, 'w') as f:
            f.write("test content")
    
    def teardown_method(self):
        """Clean up temporary files."""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
        
        # Clean up zip file
        zip_file = "test_output.zip"
        if os.path.exists(zip_file):
            os.remove(zip_file)
    
    def test_create_zip(self):
        """Test creating a ZIP archive."""
        zip_path = zip_project(folder=self.temp_dir, zip_name="test_output")
        
        assert os.path.exists("test_output.zip")
        assert zip_path == "test_output.zip"
    
    def test_zip_nonexistent_folder_raises_error(self):
        """Test that zipping a non-existent folder raises error."""
        with pytest.raises(ValueError, match="does not exist"):
            zip_project(folder="nonexistent_folder")
    
    def test_zip_empty_folder_raises_error(self):
        """Test that zipping an empty folder raises error."""
        empty_dir = tempfile.mkdtemp()
        try:
            with pytest.raises(ValueError, match="empty"):
                zip_project(folder=empty_dir)
        finally:
            os.rmdir(empty_dir)
