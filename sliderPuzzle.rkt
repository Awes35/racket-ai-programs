#lang scheme/load


; DFS call    (generalized-search (goal? finish) (extend-paths) append           (list (list start))  '()      0) ))
; BFS call    (generalized-search (goal? finish) (extend-paths) (appendToEnd)    (list(list start))   '()      0) ))
            ; (generalized-search (finish        path-extender   extend-method   path-container       closed   nodeCnt) )



; (define gg '(1 2 3 8 0 4 7 6 5))
; (define ss '(1 2 3 8 4 5 0 7 6))


(define generalized-search
  (lambda (finish path-Extender extend-method path-container closed nodeCnt)
    (newline) (newline) (newline)
    (display "Current set of visited nodes: ")
    (display closed)
    (newline)
    (display "Current Path-container: ")
    (display path-container)
    (newline)
    (cond
      ((null? path-container) (begin (display "search complete") (newline)))
      ((finish (car path-container)) 	
				(display nodeCnt)
                                (display " nodes examined")
       				(newline)
                                (display (reverse (car path-container)))
       				(newline)
       				(display ""))

      ((member? (caar path-container) closed)
       				(display "reject path " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      (else 
       (display "Extending path ")
            (display (reverse (car path-container)))
            (newline)
            ;(display "---------------")
            ;(newline)
            (generalized-search finish path-Extender extend-method
                                (extend-method (path-Extender (car path-container)) (cdr path-container))
                                (cons (caar path-container) closed)
								(+ 1 nodeCnt))))))



;;
;; 8-puzzle.scm
;;

;;
;; Describe a representation for a graph
;;
; A node in the semantic net is a 9-tuple (a b c d e f g h i).  Where
; position x of the 9-tuple indicates which tile is located there.
; The empty position is represented by a 0.  Furthermore the nine 
; locations of the puzzle are numbered 1-9; left-right, top-bottom.
;
; A valid state is one where the numbers 0-8 all appear in the 9-tuple.
;
; In any given transition, the empty position exchanges its location with
; any of its adjacent tiles.  Depending on where the empty slot is, there
; is either 4 or 3 or 2 legal transitions. (Diagonal exchanges are not 
; considered legal.)
;
; Define some handy labels for the start and goal states of the game
; (ss2 and ss3 are very hard)
(define ss '(1 2 3 8 4 5 0 7 6))
(define ss4 '(0 1 3 6 2 7 8 5 4))
(define ss2 '(2 3 8 1 7 4 6 5 0))
(define ss3 '(8 1 0 7 5 3 6 4 2)) ; i[0] = 8 .. g[3] = 8
(define gg '(1 2 3 8 0 4 7 6 5))  ; i[0][0] = 8 .. g[0][1]=8


;; Other interesting start states: all of which have solutions
; '(1 3 4 8 2 5 7 6 0)
; '(8 1 2 0 4 3 7 6 5)
; '(2 0 3 1 6 4 8 7 5)
; '(1 6 2 8 0 3 7 5 4)
; '(0 1 3 6 2 7 8 5 4)
; '(2 3 4 1 8 5 7 6 0)
;------------------------------------------------------------------------------------
;;
;; Provide a function, given a partial path and a specified representation, that will
;; create n new paths; each an extension of the provided partial path by one step.
;; Return the n new paths as a list of paths.
;;


(define extend-paths ; x=((2 3 8 1 7 4 6 5 0)) .. car x = (2 3 8 1 7 4 6 5 0) .. list-index '0 (car x) = empty 0 index, -1 if none
  (lambda () ;from initial 8p call, to be passed to generalized-search
    (lambda (x)
      (get-new-paths x (get-next-step-list (car x) (list-index '0 (car x))) ))))

(define get-new-paths ;path = ((2 3 8 1 7 4 6 5 0)) .. (2 3 8 1 7 4 6 5 0)
  ;((2 3 8 1 7 4 6 0 5) (2 3 8 1 7 4 6 5 0))
  (lambda (path next-step-list)
    (cond
      ((null? next-step-list) '())
      ((member? (car next-step-list) path) (get-new-paths path (cdr next-step-list))) ;if next step already in existing path, ignore
      (else (cons (cons (car next-step-list) path) ;
                  (get-new-paths path (cdr next-step-list)))))))

(define get-next-step-list
  (lambda (node empty-pos)
    (let ((rowDiff (quotient empty-pos 3))
          (colDiff (remainder empty-pos 3)))
     (append (newNodes node 3 rowDiff empty-pos)
             (newNodes node 1 colDiff empty-pos)
             '()))))

(define newNodes
  (lambda (node delta diff empty-pos)
    (cond 
      ((equal? diff 0) (list (swapper empty-pos (+ empty-pos delta) node) ))
      ((equal? diff 1) (append (list (swapper empty-pos (+ empty-pos delta) node))
                               (list (swapper empty-pos (- empty-pos delta) node))
                               '()))
      (else (list (swapper empty-pos (- empty-pos delta) node))))))




;;-----------------------------------------------
;;
;; Provide a function, given a partial path, that will determine if the goal state 
;; has been reached.
;;

(define goal?
  (lambda (finish)
    (lambda (x)
      (equal? finish (car x)))))


(define 8p-dfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) append (list(list start)) '() 0) ))


(define 8p-bfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) (appendToEnd) (list(list start)) '() 0) ))

;(define ss2 '(2 3 8 1 7 4 6 5 0))
;(define gg '(1 2 3 8 0 4 7 6 5))



;;------------------ Utility Functions --------------------
(define appendToEnd
  (lambda ()
	(lambda (newStuff oldStuff)
      (reverse (append newStuff (reverse oldStuff))))))



(define member? (lambda (a lat)
                  (cond
                    ((null? lat) #f)
                    (else (or (equal? (car lat) a)
                              (member? a (cdr lat)))))))

(define list-index
  (lambda (s los)
    (list-index-helper s los 0)))

(define list-index-helper
  (lambda (s los cnt)
    (cond
      ((null? los) -1)
      ((eq? s (car los)) cnt)
      (else (list-index-helper s (cdr los) (+ cnt 1))))))

(define swapper
  (lambda (pos1 pos2 slst)
    (let ((s1 (list-ref slst pos1))
          (s2 (list-ref slst pos2)))
      (map
        (lambda (selement)
          (cond
            ((eq? selement s1) s2)
            ((eq? selement s2) s1)
            (else selement)))
        slst))))



(8p-bfs ss gg)
;(8p-dfs ss gg)


