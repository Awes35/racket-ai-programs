#lang scheme/load

; 0 1 2 | 1 2 3 | 2 3 4

; a b c | d e f | g h i

;(define manhattan
 ; (lambda currState goalState
;    (

(define manhattan
  (let ((total 0))
   (lambda el currentState goalState
     (+ (truncate(/(distance(el currentState goalState))3)) (modulo((distance(el currentState goalState))3)) )
     )))


(define distance
  (lambda (el lst1 lst2)
    (let ((dst 0))
      (set! dst (abs(- (findIndex el lst1) (findIndex el lst2))))
      dst
      )))


(define findIndex
  (lambda (ele lst)
    (findIndex-helper ele lst 0)
  ))

(define findIndex-helper
    (lambda (element lst cnt)
        (cond
          [(null? lst) -1]
          [(equal? (car lst) element) cnt]
          [else
           (set! cnt (+ cnt 1))
           (findIndex-helper element (cdr lst) cnt)]
        )))



;// Katie's solution

(define remainingMoves-guess ;returns score of the total manhat distance for the state
  (lambda (finish)
    (lambda (path)
      (guess-helper (car path) (- (length finish) 1) finish)))) ;


(define guess-helper ;sums up every number from manhat-dist
  (lambda (node cnt finish) ;node=curr state (list), cnt=8 highest number (0-8), finish=goal state we want (list)
    (cond
      ((zero? cnt) 0) ;we want to hit cnt 0 as our finish
      (else (+ (manhat-dist (list-index cnt node)  ;curr index
                            (list-index cnt finish) ;index of cnt in goal state
                            (sqrt (length finish))) ;3 (dimension 3x3)
               (guess-helper node (- cnt 1) finish) ;continue until done with all num
               ))
      )))


(define manhat-dist ;calculation of distance for 1 specific number 
  (lambda (a b dimension) ;a=index of value in current list/state, b=index of value in goal list/state, dimension= (3 for 3x3)
    (let ((rowA (quotient a dimension)) ;a/3 = row we are at 
          (colA (remainder a dimension)) ;a/3 remainder = column
          (rowB (quotient b dimension))
          (colB (remainder b dimension))) ;done with let
      (+ (abs (- colB colA)) (abs (- rowB rowA))) ;distance from curr col to 
      )))


; best first search algo

(define 8p-best-first
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths)
                        (add-n-sort (remainingMoves-guess finish))
                        (list (list start)) '() 0)))

(define add-n-sort
  (lambda (sortKey)
    (lambda (newstuff oldstuff)
      (sort (append newstuff oldstuff) < #:key sortkey)
      )))
