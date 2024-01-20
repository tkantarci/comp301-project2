#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              
                              ((= op 3) (if (zero? num2)
                                            (eopl:error "zero division error")
                                            (/ num1 num2)))
                              
                              (else (- num1 num2))

                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (if (zero? num2top)
                                            (eopl:error "zero division error")
                                            (cons (* num2bot num1) num2top)))

                              (else (cons (- (* num1 num2bot) num2top) num2bot))


                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (if (zero? num2)
                                            (eopl:error "zero division error")
                                            (cons num1top (* num1bot num2))))

                              (else (cons (- num1top (* num1bot num2)) num1bot))
                              
                              ;; -----------------------
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------

                              ((= op 3) (if (zero? num2top)
                                            (eopl:error "zero division error")
                                            (cons (* num1top num2bot) (* num1bot num2top))))

                              (else
                               (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))

                              ;; ----------------------- 
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                        
                        (let ((num1Top (car num1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))

                          ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      (rational-exp (num1 num2)   
                    (if (zero? num2)
                        (eopl:error "Zero cannot be denominator.")
                        (rational-val (cons num1 num2))))

      (simpl-exp (exp)
                 (let ((num (expval->rational (value-of exp env))))
                   (if (number? num)
                       (rational-val (cons num 1))
                       (rational-val
                        (let (
                          (numTop (car num))
                          (numBottom (cdr num))
                          )
                          (let ((gcdNum (gcd numTop numBottom)))
                            
                          (cons (/ numTop gcdNum) (/ numBottom gcdNum))))))))

      (list-exp () (list-val '()))

      (cons-exp (exp1 lst)
            (let ((val1 (value-of exp1 env))
                  (val-lst (value-of lst env)))
              	(list-val (append (list (expval->num val1)) (expval->list val-lst)))))

      (sum-exp (lst)
           (let ((val-lst (expval->list (value-of lst env))))
             	(num-val (apply + val-lst))))

      (max-exp (lst)
           (let ((val-lst (expval->list (value-of lst env))))
               (num-val (max val-lst))))

      ;; -----------------------

      )))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define max-helper (lambda (max lst)
                       (if (null? lst)
                           max
                           (max-helper (if (> (car lst) max) (car lst) max) (cdr lst)))))

(define max (lambda (lst)
                (if (null? lst)
                    -1
                    (max-helper -999999 lst))))