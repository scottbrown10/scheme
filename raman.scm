(load "utilities.scm")

(define (raman n)
  (let ((nums '()))
    (for-each (lambda (x)
                (let* ((perms (permutations x))
                       (found (find (lambda (y)
                                      (=
                                        (+ (expt (car y) 3) (expt (cadr y) 3))
                                        (+ (expt (caddr y) 3) (expt (cadddr y) 3)))) perms)))
                  (when found (set! nums (append nums (list found))))))
  (subsets (iota n 1) 4))
  (map (lambda (x) (cons (+ (expt (car x) 3) (expt (cadr x) 3)) x)) nums)))
