"""
File extraction and project structure generation for Java Spring Batch code.
"""
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import re
import shutil
from datetime import datetime
from utils.logger import setup_logger

logger = setup_logger('file_writer')

def extract_files_from_response(response):
    """
    Extract Java files from the Ollama response.
    
    Args:
        response: Raw response from Ollama containing Java code blocks
        
    Returns:
        dict: Dictionary mapping filename to file content
        
    Raises:
        ValueError: If no files can be extracted
    """
    logger.info("Extracting Java files from response...")
    
    # Try multiple patterns to be more robust
    patterns = [
        r"```java filename:\s*(.*?)\n(.*?)```",  # Original pattern
        r"```java\s+filename:\s*(.*?)\n(.*?)```",  # With space
        r"```java\s*//\s*filename:\s*(.*?)\n(.*?)```",  # With comment
        r"```java\s*//\s*(.*?\.java)\n(.*?)```",  # Just .java extension
    ]
    
    file_dict = {}
    
    for pattern in patterns:
        matches = re.findall(pattern, response, re.DOTALL | re.IGNORECASE)
        for filename, content in matches:
            filename = filename.strip()
            content = content.strip()
            
            # Ensure .java extension
            if not filename.endswith('.java'):
                filename += '.java'
            
            # Validate content
            if content and len(content) > 10:  # Minimum content length
                file_dict[filename] = content
                logger.info(f"  ✓ Extracted {filename} ({len(content)} chars)")
    
    if not file_dict:
        logger.warning("No files extracted with standard patterns, trying fallback...")
        # Fallback: try to extract any Java code blocks
        code_blocks = re.findall(r"```java\n(.*?)```", response, re.DOTALL)
        for i, code in enumerate(code_blocks):
            if code.strip() and len(code.strip()) > 10:
                filename = f"GeneratedCode{i+1}.java"
                file_dict[filename] = code.strip()
                logger.info(f"  ✓ Extracted {filename} (fallback)")
    
    if not file_dict:
        error_msg = "No valid Java files found in the response. The model may not have generated code in the expected format."
        logger.error(error_msg)
        raise ValueError(error_msg)
    
    logger.info(f"Successfully extracted {len(file_dict)} Java file(s)")
    return file_dict

def cleanup_old_output(base_dir):
    """
    Clean up old output directory if it exists.
    
    Args:
        base_dir: Path to output directory
    """
    if os.path.exists(base_dir):
        try:
            logger.info(f"Cleaning up old output directory: {base_dir}")
            shutil.rmtree(base_dir)
        except Exception as e:
            logger.warning(f"Could not clean up old directory: {e}")

def write_files_to_project(file_dict, base_dir="output_project/src/main/java/com/example/batch"):
    """
    Write extracted Java files to the project directory.
    
    Args:
        file_dict: Dictionary mapping filename to content
        base_dir: Base directory for Java files
        
    Returns:
        int: Number of files written
    """
    logger.info(f"Writing {len(file_dict)} files to {base_dir}...")
    
    os.makedirs(base_dir, exist_ok=True)
    files_written = 0
    
    for filename, content in file_dict.items():
        try:
            file_path = os.path.join(base_dir, filename)
            with open(file_path, "w", encoding='utf-8') as f:
                f.write(content)
            logger.info(f"  ✓ Wrote {filename}")
            files_written += 1
        except Exception as e:
            logger.error(f"  ✗ Failed to write {filename}: {e}")
    
    logger.info(f"Successfully wrote {files_written}/{len(file_dict)} files")
    return files_written

def write_pom(output_dir="output_project"):
    """
    Write a comprehensive Spring Batch pom.xml file.
    
    Args:
        output_dir: Directory where pom.xml will be written
    """
    logger.info("Generating pom.xml...")
    
    pom_xml = """<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>2.7.18</version>
        <relativePath/>
    </parent>
    
    <groupId>com.example</groupId>
    <artifactId>cobol-to-java-batch</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    
    <name>COBOL to Java Spring Batch</name>
    <description>Spring Batch project generated from COBOL code</description>

    <properties>
        <java.version>11</java.version>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- Spring Boot Starter Batch -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-batch</artifactId>
        </dependency>
        
        <!-- Spring Boot Starter -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter</artifactId>
        </dependency>
        
        <!-- H2 Database for JobRepository -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <scope>runtime</scope>
        </dependency>
        
        <!-- Lombok (optional, for reducing boilerplate) -->
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>
        
        <!-- Testing -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>org.springframework.batch</groupId>
            <artifactId>spring-batch-test</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
                <configuration>
                    <excludes>
                        <exclude>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                        </exclude>
                    </excludes>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
"""
    
    os.makedirs(output_dir, exist_ok=True)
    pom_path = os.path.join(output_dir, "pom.xml")
    
    try:
        with open(pom_path, "w", encoding='utf-8') as f:
            f.write(pom_xml)
        logger.info(f"  ✓ Generated pom.xml at {pom_path}")
    except Exception as e:
        logger.error(f"  ✗ Failed to write pom.xml: {e}")
        raise

def write_readme(output_dir="output_project"):
    """
    Write a README file for the generated project.
    
    Args:
        output_dir: Directory where README will be written
    """
    logger.info("Generating README.md for output project...")
    
    readme_content = f"""# Spring Batch Project

This Spring Batch project was automatically generated from COBOL code on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}.

## Prerequisites

- Java 11 or higher
- Maven 3.6 or higher

## Build

```bash
mvn clean install
```

## Run

```bash
mvn spring-boot:run
```

## Project Structure

- `src/main/java/com/example/batch/` - Java source files
- `pom.xml` - Maven project configuration

## Notes

- This is an auto-generated project and may require adjustments
- Review and test the code before using in production
- Configure input/output file paths as needed
- Adjust batch job parameters in BatchConfig.java

## Support

For issues or questions about the COBOL to Java converter, please refer to the main project repository.
"""
    
    readme_path = os.path.join(output_dir, "README.md")
    
    try:
        with open(readme_path, "w", encoding='utf-8') as f:
            f.write(readme_content)
        logger.info(f"  ✓ Generated README.md")
    except Exception as e:
        logger.warning(f"  ⚠ Could not write README.md: {e}")

def zip_project(folder="output_project", zip_name="spring_batch_output"):
    """
    Create a ZIP archive of the generated project.
    
    Args:
        folder: Directory to zip
        zip_name: Name of the zip file (without extension)
        
    Returns:
        str: Path to the created zip file
        
    Raises:
        ValueError: If folder doesn't exist or is empty
    """
    if not os.path.exists(folder):
        error_msg = f"Output folder {folder} does not exist"
        logger.error(error_msg)
        raise ValueError(error_msg)
    
    if not os.listdir(folder):
        error_msg = f"Output folder {folder} is empty"
        logger.error(error_msg)
        raise ValueError(error_msg)
    
    logger.info(f"Creating ZIP archive: {zip_name}.zip")
    
    try:
        # Remove old zip if exists
        zip_path = f"{zip_name}.zip"
        if os.path.exists(zip_path):
            os.remove(zip_path)
        
        shutil.make_archive(zip_name, 'zip', folder)
        file_size = os.path.getsize(zip_path)
        logger.info(f"  ✓ Created {zip_path} ({file_size} bytes)")
        return zip_path
    except Exception as e:
        logger.error(f"  ✗ Failed to create ZIP: {e}")
        raise

