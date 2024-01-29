(define twoOperatorCalculator (lambda (lst) (
      cond
      ((null? lst) ())
      ((null? (cdr lst)) (car lst))
      ((eq? '+ (cadr lst)) (twoOperatorCalculator (cons (+ (car lst) (caddr lst)) (cdddr lst))))
      ((eq? '- (cadr lst)) (twoOperatorCalculator (cons (- (car lst) (caddr lst)) (cdddr lst)))) 
    )
  )
)

(define fourOperatorCalculator (lambda (lst) (
      cond
      ((null? lst) (list))
      ((null? (cdr lst)) (list (car lst)))
      ((eq? (cadr lst) '+) (append (list (car lst) (cadr lst)) (fourOperatorCalculator (cddr lst))))
      ((eq? (cadr lst) '-) (append (list (car lst) (cadr lst)) (fourOperatorCalculator (cddr lst))))   
      ((eq? (cadr lst) '*) (fourOperatorCalculator (cons (* (car lst) (caddr lst)) (cdddr lst))))
      ((eq? (cadr lst) '/) (fourOperatorCalculator (cons (/ (car lst) (caddr lst)) (cdddr lst))))
      )
  ) 
)


(define calculatorNested (lambda (lst) (
  cond
  ((null? lst) (list))
  ((list? (car lst)) (calculatorNested (append (list (twoOperatorCalculator (fourOperatorCalculator (calculatorNested (car lst))))) (cdr lst))))
  ((null? (cdr lst)) (list (car lst)))
  ((eq? (cadr lst) '+) (append (list (car lst) (cadr lst)) (calculatorNested (cddr lst))))
  ((eq? (cadr lst) '-) (append (list (car lst) (cadr lst)) (calculatorNested (cddr lst))))
  ((eq? (cadr lst) '*) (append (list (car lst) (cadr lst)) (calculatorNested (cddr lst))))
  ((eq? (cadr lst) '/) (append (list (car lst) (cadr lst)) (calculatorNested (cddr lst))))
    )
  )
)

(define checkOperators (lambda (lst)(
      cond
      ((not (list? lst)) #f)
      ((null? lst) #f)
      ((list? (car lst)) (and (checkOperators (car lst)) (checkOperators (append (list 1) (cdr lst)))))
      ((null? (cdr lst)) (number? (car lst)))
      ((eq? (cadr lst) '+) (and (number? (car lst)) (checkOperators (cddr lst))))
      ((eq? (cadr lst) '-) (and (number? (car lst)) (checkOperators (cddr lst))))
      ((eq? (cadr lst) '*) (and (number? (car lst)) (checkOperators (cddr lst))))
      ((eq? (cadr lst) '/) (and (number? (car lst)) (checkOperators (cddr lst))))
      (else #f)
    )
  )
)
(define calculator (lambda (lst) (
    cond
    ((checkOperators lst) (twoOperatorCalculator (fourOperatorCalculator (calculatorNested lst))))
    (else #f)
    )
  )
)