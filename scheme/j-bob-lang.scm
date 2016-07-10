
(define s.car car)
(define s.cdr cdr)
(define s.+ +)
(define s.< <)
(define (num x) (if (number? x) x 0))
(define (if/nil Q A E)
  (if (equal? Q 'nil) (E) (A)))

(define (atom x) (if (pair? x) 'nil 't))
(define (car x) (if (pair? x) (s.car x) '()))
(define (cdr x) (if (pair? x) (s.cdr x) '()))
(define (equal x y) (if (equal? x y) 't 'nil))
(define (natp x)
  (if (integer? x) (if (s.< x 0) 'nil 't) 'nil))
(define (+ x y) (s.+ (num x) (num y)))
(define (< x y)
  (if (s.< (num x) (num y)) 't 'nil))

(define-syntax if
    ((if ?Q ?A ?E)
     (if/nil ?Q (lambda () ?A) (lambda () ?E))))

(define-syntax defun
  ((defun ?name () ?body) (define (?name) ?body))
  ((defun ?name (?arg1) ?body) (define (?name ?arg1) ?body))
  ((defun ?name (?arg1 ?arg2) ?body) (define (?name ?arg1 ?arg2) ?body))
  ((defun ?name (?arg1 ?arg2 ?arg3) ?body) (define (?name ?arg1 ?arg2 ?arg3) ?body))
  ((defun ?name (?arg1 ?arg2 ?arg3 ?arg4) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4) ?body))
  ((defun ?name (?arg1 ?arg2 ?arg3 ?arg4 ?arg5) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4 ?arg5) ?body))
  ((defun ?name (?arg1 ?arg2 ?arg3 ?arg4 ?arg5 ?arg6) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4 ?arg5 ?arg6) ?body)))


  (define-syntax dethm
    ((dethm ?name () ?body) (define (?name) ?body))
    ((dethm ?name (?arg1) ?body) (define (?name ?arg1) ?body))
    ((dethm ?name (?arg1 ?arg2) ?body) (define (?name ?arg1 ?arg2) ?body))
    ((dethm ?name (?arg1 ?arg2 ?arg3) ?body) (define (?name ?arg1 ?arg2 ?arg3) ?body))
    ((dethm ?name (?arg1 ?arg2 ?arg3 ?arg4) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4) ?body))
    ((dethm ?name (?arg1 ?arg2 ?arg3 ?arg4 ?arg5) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4 ?arg5) ?body))
    ((dethm ?name (?arg1 ?arg2 ?arg3 ?arg4 ?arg5 ?arg6) ?body) (define (?name ?arg1 ?arg2 ?arg3 ?arg4 ?arg5 ?arg6) ?body)))


(defun size (x)
  (if (atom x)
    '0
    (+ '1 (+ (size (car x)) (size (cdr x))))))
