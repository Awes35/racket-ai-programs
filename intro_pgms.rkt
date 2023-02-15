#lang racket

;author: Kollen Gruizenga
;CS370


(define atom?
  (lambda (arg)
    (and (not (pair? arg))
         (not (null? arg))
         )
    ))


;prob1~ returns true if ATM in LST
(define member?
  (lambda (atm lst)
    (cond
      [(null? lst) #f]
      [(equal? (car lst) atm) #t]
      [else (member? atm (cdr lst)) ]
      )))

;prob2~ returns LST without first occurrence of DEXP
(define rember
  (let ((lstPre '() ))
    (lambda (dexp lst)
      (cond
        [(null? lst) lstPre] ;got through all of lst 
        [(equal? (car lst) dexp) ;if currLetter = dexp
          (set! lstPre (append lstPre (cdr lst)))
          (rember dexp '()) ] ;return lstPre + rest of lst
        [else ;currLetter != dexp
         (set! lstPre (append lstPre (list (car lst)))) ;lstPre + currLetter
         (rember dexp (cdr lst)) ] ;call rember(dexp, remainingList)
        ))))

;prob3~ returns first atomic element in list
(define burrow
  (lambda (lst)
    (cond
      [(null? lst) '()] ;check if lst has anything else
      [(list? (car lst)) (burrow (car lst)) ] ;if element is another list, then call on sublist
      [(atom? (car lst)) (car lst) ] ;if element is atom, return
      [else (burrow (cdr lst))] ;else go to next element
      )))

;prob4~ returns list containing same dexps as non-null input list, in same order except last dexp
(define butLast
  (lambda (lst)
    (cond
      [(null? (cdr lst)) '()]
      [else (append (list (car lst)) (butLast (cdr lst)) )]
      )))

;prob5~ returns list containing N copies of X
(define duple
  (let ((r_Lst '() ))
    (lambda (n x)
      (cond
        [(= n 0) r_Lst]
        [else
         (set! r_Lst (append r_Lst (list x)) )
         (duple (- n 1) x) ]
        ))))
