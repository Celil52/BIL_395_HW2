;;; Simple Calculator Interpreter in Scheme

;; Variable environment
(define variables (make-hash-table))

;; Token types
(define TOKEN-NUMBER 0)
(define TOKEN-IDENTIFIER 1)
(define TOKEN-PLUS 2)
(define TOKEN-MINUS 3)
(define TOKEN-MULTIPLY 4)
(define TOKEN-DIVIDE 5)
(define TOKEN-EQUALS 6)
(define TOKEN-LPAREN 7)
(define TOKEN-RPAREN 8)
(define TOKEN-EOF 9)

;; Token constructor
(define (make-token type value)
  (cons type value))

;; Token accessors
(define (token-type token) (car token))
(define (token-value token) (cdr token))

;; Lexer
(define (make-lexer input)
  (let ((text input)
        (pos 0)
        (curr-char (if (string=? input "") #f (string-ref input 0))))
    (define (advance)
      (set! pos (+ pos 1))
      (if (< pos (string-length text))
          (set! curr-char (string-ref text pos))
          (set! curr-char #f)))
    
    (define (skip-whitespace)
      (while (and curr-char (char-whitespace? curr-char))
        (advance)))
    
    (define (read-number)
      (let ((start-pos pos)
            (has-decimal #f))
        (while (and curr-char
                    (or (char-numeric? curr-char)
                        (and (char=? curr-char #\.) (not has-decimal))))
          (when (char=? curr-char #\.)
            (set! has-decimal #t))
          (advance))
        (let ((number-str (substring text start-pos pos)))
          (string->number number-str))))
    
    (define (read-identifier)
      (let ((start-pos pos))
        (while (and curr-char
                    (or (char-alphabetic? curr-char)
                        (char-numeric? curr-char)
                        (char=? curr-char #\_)))
          (advance))
        (substring text start-pos pos)))
    
    (define (get-next-token)
      (skip-whitespace)
      (cond
        ((not curr-char) (make-token TOKEN-EOF #f))
        ((char-numeric? curr-char)
         (let ((num (read-number)))
           (make-token TOKEN-NUMBER num)))
        ((char-alphabetic? curr-char)
         (let ((id (read-identifier)))
           (make-token TOKEN-IDENTIFIER id)))
        ((char=? curr-char #\+)
         (advance)
         (make-token TOKEN-PLUS "+"))
        ((char=? curr-char #\-)
         (advance)
         (make-token TOKEN-MINUS "-"))
        ((char=? curr-char #\*)
         (advance)
         (make-token TOKEN-MULTIPLY "*"))
        ((char=? curr-char #\/)
         (advance)
         (make-token TOKEN-DIVIDE "/"))
        ((char=? curr-char #\=)
         (advance)
         (make-token TOKEN-EQUALS "="))
        ((char=? curr-char #\()
         (advance)
         (make-token TOKEN-LPAREN "("))
        ((char=? curr-char #\))
         (advance)
         (make-token TOKEN-RPAREN ")"))
        (else
         (error (string-append "Invalid character: " (string curr-char))))))
    
    ;; Return the lexer interface
    (lambda (op)
      (case op
        ((get-next-token) (get-next-token))))))

;; Parser
(define (make-parser lexer)
  (let ((current-token (lexer 'get-next-token)))
    
    (define (error message)
      (error message))
    
    (define (eat token-type)
      (if (= (token-type current-token) token-type)
          (set! current-token (lexer 'get-next-token))
          (error (string-append "Unexpected token: " (token-value current-token)))))
    
    (define (factor)
      (let ((token current-token))
        (cond
          ((= (token-type token) TOKEN-NUMBER)
           (eat TOKEN-NUMBER)
           (token-value token))
          ((= (token-type token) TOKEN-IDENTIFIER)
           (let ((var-name (token-value token)))
             (eat TOKEN-IDENTIFIER)
             (if (hash-table-exists? variables var-name)
                 (hash-table-ref variables var-name)
                 (error (string-append "Undefined variable: " var-name)))))
          ((= (token-type token) TOKEN-LPAREN)
           (eat TOKEN-LPAREN)
           (let ((result (expr)))
             (eat TOKEN-RPAREN)
             result))
          (else
           (error "Invalid syntax")))))
    
    (define (term)
      (let ((result (factor)))
        (while (or (= (token-type current-token) TOKEN-MULTIPLY)
                   (= (token-type current-token) TOKEN-DIVIDE))
          (let ((token current-token))
            (cond
              ((= (token-type token) TOKEN-MULTIPLY)
               (eat TOKEN-MULTIPLY)
               (set! result (* result (factor))))
              ((= (token-type token) TOKEN-DIVIDE)
               (eat TOKEN-DIVIDE)
               (let ((divisor (factor)))
                 (if (zero? divisor)
                     (error "Division by zero")
                     (set! result (/ result divisor))))))))
        result))
    
    (define (expr)
      ;; Check for assignment
      (if (= (token-type current-token) TOKEN-IDENTIFIER)
          (let ((var-name (token-value current-token)))
            (eat TOKEN-IDENTIFIER)
            (if (= (token-type current-token) TOKEN-EQUALS)
                (begin
                  (eat TOKEN-EQUALS)
                  (let ((value (expr)))
                    (hash-table-set! variables var-name value)
                    value))
                ;; Not an assignment, need to reset and reprocess
                (begin
                  ;; Rebuild the lexer with the var-name prepended
                  (set! lexer (make-lexer var-name))
                  (set! current-token (lexer 'get-next-token))
                  (expr)))))
          
          (let ((result (term)))
            (while (or (= (token-type current-token) TOKEN-PLUS)
                       (= (token-type current-token) TOKEN-MINUS))
              (let ((token current-token))
                (cond
                  ((= (token-type token) TOKEN-PLUS)
                   (eat TOKEN-PLUS)
                   (set! result (+ result (term))))
                  ((= (token-type token) TOKEN-MINUS)
                   (eat TOKEN-MINUS)
                   (set! result (- result (term)))))))
            result)))
    
    (define (parse)
      (expr))
    
    ;; Return the parser interface
    (lambda (op)
      (case op
        ((parse) (parse))))))

;; Helper definition for while loops
(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let loop ()
       (when test
         body ...
         (loop))))))

;; Main interpreter loop
(define (calculator-interpreter)
  (display "Scheme Calculator Interpreter\n")
  (display "Enter expressions or 'quit'/'exit' to quit\n")
  
  (let loop ()
    (display "> ")
    (let ((input (read-line)))
      (cond
        ((or (string=? input "quit") (string=? input "exit"))
         (display "Goodbye!\n"))
        ((string=? input "")
         (loop))
        (else
         (with-exception-handler
          (lambda (exn)
            (display "Error: ")
            (display (if (message-condition? exn)
                         (condition-message exn)
                         "Unknown error"))
            (newline))
          (lambda ()
            (let* ((lexer (make-lexer input))
                   (parser (make-parser lexer))
                   (result (parser 'parse)))
              (display result)
              (newline))))
         (loop))))))

;; Start the interpreter
(calculator-interpreter) 