#lang eopl

;; grammar for the LET language  

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)

    ;;
    ;; -----------------------
    ;; INSERT YOUR CODE HERE 
    ;; -----------------------

    ;; -----------------------
  ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   

    ;; -----------------------
    ;; INSERT YOUR CODE HERE 
    ;; -----------------------
    
    (expression
     ("(" number "/" number ")")
     rational-exp)
    
    (expression
     ("op" "(" expression "," expression "," number ")")
     op-exp)
    
    (expression
     ("create-new-list" "()") 
     list-exp)
    (expression
     ("cons"  expression "to" expression)
     cons-exp)

    (expression
     ("sum" "(" expression ")")
     sum-exp)

    (expression
     ("max" "(" expression ")")
     max-exp)
    
    (expression
     (identifier)
      var-exp)

    (expression
     ("simpl" "(" expression ")")
     simpl-exp)


    ;; -----------------------
))


;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

