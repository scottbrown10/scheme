(load "utilities.scm")

(define lst '())
(define s (fact 9))

(define (pandigital n)
  (let ((digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (and (= (string-length (number->string n)) 10)
         (every (lambda (digit)
          (member digit (string->list (number->string n)))) digits)
         )))
(do ((x s (+ s x)))
       ((> (string-length (number->string x) ) 10))
       (if (pandigital x)
         (set! lst (cons x lst))))

(display (sort lst <))
