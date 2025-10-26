(define (fib (n : int) : int)
    (if (<= n 1)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))


(define (fib2 (n : int) (a : int) (b : int) : int) (if (<= n 1) b (fib2 (- n 1) b (+ a b))))

(define (main : ()) (
    let ([x (read)])
    (begin (print_int x)
    (print_int (fib x))
    (print_int (fib2 x 1 1)))))
