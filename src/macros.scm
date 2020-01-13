;; globals defined this way will only run the body once.
;; This lets you set up global parameters that only calculate themselves when needed.
;; The global can be redefined by passing in #t as an argument.
;;
;; Example: Suppose (big-list) returns a large list that takes a long time to compute.
;; Not every run of the program will need big-list, but it's needed for multiple functions.
;; Defining big-list this way means that the first time it's called, it'll do the big calculation
;; and return it. Every call after that will just recall that result.
(define-syntax define-once
  (syntax-rules ()
    ((_ name . body)
     (define name
       (let ((init? #f) (x #f))
	 (lambda (#!optional rerun?)
           (when (or (not init?) rerun?)
             (set! init? #t)
             (set! x (begin . body)))
           x))))))
