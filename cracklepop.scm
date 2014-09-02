; Copyright (c) Sak Lee 2014 All rights reserved
; written in R5RS Scheme
;
; sequence generator macro, assuming that we don't have the implementation
; of SRFI-42 (eager comprehension) to generate numerical sequence
(define-syntax generate
  (syntax-rules ()
    ((generate start end)
       (generate start end 1))
    ((generate start end step)
       (letrec ((gen
                  (lambda (s e st comp)
                    (if (comp s e)
                      '()
                      (cons s (gen (+ s st) e st comp)))))
                (compfun
                  (cond ((> step 0) >)
                        ((< step 0) <)
                        (else (error "0 is not a valid step")))))
         (gen start end step compfun)))))

; function composition macro. ((compose f g) x) is equal to (f (g (x)))
(define-syntax compose
  (syntax-rules ()
    ((compose)
       (lambda args
         (apply values args)))
    ((compose f . fs)
       (lambda args
         (f (apply (compose . fs) args))))))

; a function that receives a number and creates a list of items to print
(define (cracklepop num)
  (letrec
    ((cpop
       (lambda (number divisible cp-pairs)
         (if (null? cp-pairs)
             (if divisible
                 '(#\newline)
                 `(,number #\newline))
             (let* ((divisor (caar cp-pairs))
                    (message (cadar cp-pairs))
                    (new-divisible (= 0 (remainder number divisor)))
                    (deliver (lambda (x) (if new-divisible (cons message x) x))))
               (deliver (cpop number (or divisible new-divisible) (cdr cp-pairs))))))))
    (cpop num #f '((3 "Crackle") (5 "Pop")))))

; a function that prints everything in a given list
(define (print-all lst)
  (for-each display lst))

; the main program
(define (main)
  (for-each (compose print-all cracklepop) (generate 1 100)))

(main)
