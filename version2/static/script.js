// Client-side JavaScript for enhanced UX

document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('conversionForm');
    const loadingOverlay = document.getElementById('loadingOverlay');
    const cobolTextarea = document.getElementById('cobolCode');
    
    // Show loading overlay on form submit
    if (form) {
        form.addEventListener('submit', function(e) {
            const cobolCode = cobolTextarea.value.trim();
            
            if (!cobolCode) {
                e.preventDefault();
                alert('Please enter COBOL code to convert');
                return;
            }
            
            // Show loading overlay
            if (loadingOverlay) {
                loadingOverlay.classList.add('active');
            }
        });
    }
    
    // Copy to clipboard functionality
    window.copyToClipboard = function(filename) {
        const pre = document.querySelector(`[data-file="${filename}"] pre`);
        if (!pre) return;
        
        const text = pre.textContent;
        navigator.clipboard.writeText(text).then(function() {
            // Show success feedback
            const btn = event.target;
            const originalText = btn.textContent;
            btn.textContent = '✓ Copied!';
            btn.style.backgroundColor = '#28a745';
            btn.style.color = 'white';
            btn.style.borderColor = '#28a745';
            
            setTimeout(function() {
                btn.textContent = originalText;
                btn.style.backgroundColor = '';
                btn.style.color = '';
                btn.style.borderColor = '';
            }, 2000);
        }).catch(function(err) {
            console.error('Failed to copy:', err);
            alert('Failed to copy to clipboard');
        });
    };
    
    // Load example COBOL code
    window.loadExample = function(exampleType) {
        let exampleCode = '';
        
        if (exampleType === 'simple') {
            exampleCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROG.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05 IN-ID            PIC 9(5).
           05 IN-NAME          PIC X(30).
           05 IN-AMOUNT        PIC 9(7)V99.
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05 OUT-ID           PIC 9(5).
           05 OUT-NAME         PIC X(30).
           05 OUT-AMOUNT       PIC 9(7)V99.
       
       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       PROCESS-RECORD.
           MOVE IN-ID TO OUT-ID
           MOVE IN-NAME TO OUT-NAME
           ADD 100 TO IN-AMOUNT GIVING OUT-AMOUNT
           WRITE OUTPUT-RECORD.`;
        } else if (exampleType === 'filter') {
            exampleCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-PROG.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC.
           05 RECORD-ID        PIC 9(5).
           05 RECORD-STATUS    PIC X.
           05 RECORD-AMOUNT    PIC 9(7)V99.
       
       FD  OUTPUT-FILE.
       01  OUTPUT-REC          PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF              PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT INPUT-FILE OUTPUT OUTPUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF RECORD-STATUS = 'A' AND RECORD-AMOUNT > 1000
                       WRITE OUTPUT-REC FROM INPUT-REC
                   END-IF
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.`;
        }
        
        if (exampleCode && cobolTextarea) {
            cobolTextarea.value = exampleCode;
            cobolTextarea.focus();
        }
    };
    
    // Character counter for COBOL textarea
    if (cobolTextarea) {
        const counter = document.getElementById('charCounter');
        
        cobolTextarea.addEventListener('input', function() {
            if (counter) {
                const length = this.value.length;
                const maxLength = 100000;
                counter.textContent = `${length.toLocaleString()} / ${maxLength.toLocaleString()} characters`;
                
                if (length > maxLength * 0.9) {
                    counter.style.color = '#dc3545';
                } else {
                    counter.style.color = '#6c757d';
                }
            }
        });
    }
});
