(define (rot-n s n)
  (define (rot-n c)
    (let ((int (char->integer c)))
    (cond ((or (> int 123) (< int 64)) int)
          (else
            (set! int (+ n int))
            (if (< int 123) int (- int 26))))))
  (list->string (map (lambda (c) (integer->char (rot-n c))) (string->list (string-downcase s)))))

(define (guess-rot s)
  (car (sort
         (map (lambda (n) (let ((rotated (rot-n s n))) (list (- 26 n) rotated (confidence rotated)))) (iota 25 1))
         (lambda(x y) (> (caddr x) (caddr y))))))

(define (confidence s)
  (/ (count vowel? (string->list s)) (string-length s)))

(define (vowel? c)
  (member c '(#\a #\e #\i #\o #\u)))
