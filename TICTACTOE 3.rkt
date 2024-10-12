#lang racket
;Subst* will be used to substitue the items of the board with Xs or Os
(define (atom? a)
  (cond
    ((list? a) #f)
    (else #t)))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst*  new old (car l)) (subst* new old (cdr l)))))))
; Define all 8 win conditions to then create a big fuction of all wins which is the base case of the play game function
(define (win1 lst)
  (cond
    ((and (eq? (car (car lst)) (car (cadr lst))) (and (eq? (car (cadr lst)) (car (caddr lst))))))
    (else #f)))
  
(define (win2 lst)
  (cond
    ((and (eq? (cadr (car lst)) (cadr (cadr lst))) (and (eq? (cadr (cadr lst)) (cadr (caddr lst))))))
    (else #f)))
         
(define (win3 lst)
  (cond
    ((and (eq? (caddr (car lst)) (caddr (cadr lst))) (and (eq? (caddr (cadr lst)) (caddr (caddr lst))))))
    (else #f)))
  
(define (win4 lst)
  (cond
    ((and (eq? (car (car lst)) (cadr (car lst))) (and (eq? (cadr (car lst)) (caddr (car lst))))))
    (else #f)))

(define (win5 lst)
  (cond
    ((and (eq? (car (cadr lst)) (cadr (cadr lst))) (and (eq? (cadr (cadr lst)) (caddr (cadr lst))))))
    (else #f)))

(define (win6 lst)
  (cond
    ((and (eq? (car (caddr lst)) (cadr (caddr lst))) (and (eq? (cadr (caddr lst)) (caddr (caddr lst))))))
    (else #f)))

(define (win7 lst)
  (cond
    ((and (eq? (car (car lst)) (cadr (cadr lst))) (and (eq? (cadr (cadr lst)) (caddr (caddr lst))))))
    (else #f)))

(define (win8 lst)
  (cond
    ((and (eq? (caddr (car lst)) (cadr (cadr lst))) (and (eq? (cadr (cadr lst)) (car (caddr lst))))))
    (else #f)))

(define (all-wins? lst)
  (cond
    ((or
      (win1 lst)
      (win2 lst)
      (win3 lst)
      (win4 lst)
      (win5 lst)
      (win6 lst)
      (win7 lst)
      (win8 lst)))
    (else #f)))
;Second base case which defines a tie asking each place has a number to it if not then it would lead to a tie
(define (tie lst)
  (cond
    ((or
        (eq? (car (car lst)) '1)
        (eq? (cadr (car lst))'2)
        (eq? (caddr (car lst))'3)
       (eq? (car (cadr lst))'4)
       (eq? (cadr (cadr lst))'5)
       (eq? (caddr (cadr lst))'6)
       (eq? (car (caddr lst))'7)
      (eq? (cadr (caddr lst))'8)
      (eq? (caddr (caddr lst))'9)))
    (else #f)))
;Combined all elements to create play game which allows you to play the tic tac toe game      
(define play-game
  (lambda (board)
   (display "Player 1 'X'") (newline)
    (let ((x (read)))
      (subst* 'X x board)
      (let ((gamebrd (subst* 'X x board)))
        (display (car gamebrd))
        (newline)
        (display (cadr gamebrd))
        (newline)
        (display (caddr gamebrd))
        (newline)
        (cond
          ((eq? (all-wins? gamebrd) #t) (display "YOU WIN"))
          ((eq? (tie gamebrd) #f) (display "YOU TIED"))
          (else (display "Player 2 'O'") (newline)
                (let ((y (read)))
                  (subst* 'O y gamebrd)
                  (let ((gameboard (subst* 'O y gamebrd)))
                    (display (car gameboard))
                    (newline)
                    (display (cadr gameboard))
                    (newline)
                    (display (caddr gameboard))
                    (newline)
                    (cond
                     ((eq? (all-wins? gameboard) #t) (display "YOU WIN"))
                     ((eq? (tie gameboard) #f) (display "YOU TIED"))
                      (else (play-game gameboard)))))))))))

(play-game '((1 2 3) (4 5 6) (7 8 9)))


