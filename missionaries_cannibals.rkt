#lang scheme/load

;author: Kollen Gruizenga
;missionaries_cannibals.rkt
; Missionaries & Cannibals - AI Project


; Program composes a "path-container" which is a list of "paths".
; Each "path" is a list of states in order
; When taking "car path" aka "caar path-container", you get the
; first state from the first path. Once done, it will display the first found possible
; path to reach the goal state using the specified search method (BFS or DFS).
;
;Goal is to traverse the possible paths to move all the people (3 missionaries, 3 cannibals)
; from the left side of the river (0) to the right side (1), using a boat that can
; carry either 1 or 2 people. The boat position is represented by the 0-left side, or 1-right side.
;
; The condition is such that there is never a state where Cannibals outnumber Missionaries
; on either side of the river. 
;
; In any given transition, the top state will try to be extended to each of
; its possible next-steps, and the search algorithm will check as it moves
; down a step whether that step is valid (M >= C on both sides).

; State is represented as (# Missionaries Left, # Cannibals Left, Boat Position (0/1), # Missionaries Right, # Cannibals Right)

;Start & Goal state:
(define ss '(3 3 0 0 0)) ; = {MMMCCC,_,L} where {left,right,boat}
(define gg '(0 0 1 3 3)) ; = {_,MMMCCC,R} where {left,right,boat}


;generalized search function, adapted to check for empty list 
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
      
      ((finish (car path-container)) ;if
				(display nodeCnt)
                                (display " nodes examined")
       				(newline)
                                (display (reverse (car path-container)))
       				(newline)
       				(display ""))
      
      ((null? (caar path-container)) ;if an individual state is empty list () -- meaning there weren't enough C or M to complete that particular state move
                                (display "empty list " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed nodeCnt)) ;don't count node as visited

      ((member? (caar path-container) closed) ;if already in visited states path
       				(display "reject path " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))
      
      ((not (valid-state? (caar path-container))) ;if cannibals outnumber missionaries on a side? (NOT valid state)
                                (display "reject path - cannibals outnumber missionaries")
                                (display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      (else
       (display "Extending path ")
            (display (reverse (car path-container)))
            (newline)
            (generalized-search finish path-Extender extend-method
                                (extend-method (path-Extender (car path-container)) (cdr path-container)) ;path-container .. extend(newStuff oldStuff)
                                (cons (caar path-container) closed) ;closed = "visited states/nodes"
								(+ 1 nodeCnt)) ;nodeCnt
            ))))

;------------------------------------------------------------------------------------
;; Functions that, when given a partial path, represent the next possible steps
;; in moving from that path (each of the states move options, or empty '() if not possible)

;given state {#m #c B #m #c}

(define extend-paths
  (lambda () ;from initial mission-cannibals-dfs call, to be passed to generalized-search
    (lambda (init_path)
      (get-new-paths init_path (get-list-next-steps (car init_path)) ))))

(define get-new-paths ;path = ((2 3 8 1 7 4 6 5 0)) .. (2 3 8 1 7 4 6 5 0)  ;path2 = ((2 3 8 1 7 4 6 0 5) (2 3 8 1 7 4 6 5 0))
  (lambda (path next-step-list)
    (cond
      ((null? next-step-list) '())
      ((member? (car next-step-list) path) (get-new-paths path (cdr next-step-list))) ;if next step already in existing path, ignore
      (else (cons (cons (car next-step-list) path) ;
                  (get-new-paths path (cdr next-step-list)))))))

;helper func to reset compiled list variable
(define get-list-next-steps
  (lambda (state)
    (get-list-next-steps-helper state '() )))
    
; given particular state, compile new list of next possible steps
(define get-list-next-steps-helper
  (lambda (state compLst) 
    (set! compLst (append compLst (list (gen-step state 1))))
    (set! compLst (append compLst (list (gen-step state 2))))
    (set! compLst (append compLst (list (gen-step state 3))))
    (set! compLst (append compLst (list (gen-step state 4))))
    (set! compLst (append compLst (list (gen-step state 5))))
    compLst))

(define gen-step
  (lambda (lst n)
    (let ((m_left (car lst))
          (c_left (car (cdr lst)))
          (boat (car (cdr (cdr lst))))
          (m_right (car (cdr (cdr (cdr lst)))))
          (c_right (car (cdr (cdr (cdr (cdr lst)))))))
    (cond
      [(equal? boat 0) ;boat 0 = left
       (cond
         ;take 1M from left
         [(and (= n 1) (>= m_left 1))
          (list (- m_left 1) c_left (+ boat 1) (+ m_right 1) c_right)]
         ;take 2M from left
         [(and (= n 2) (>= m_left 2))
          (list (- m_left 2) c_left (+ boat 1) (+ m_right 2) c_right)]
         ;take 1C from left
         [(and (= n 3) (>= c_left 1))
          (list m_left (- c_left 1) (+ boat 1) m_right (+ c_right 1))]
         ;take 2C from left
         [(and (= n 4) (>= c_left 2))
          (list m_left (- c_left 2) (+ boat 1) m_right (+ c_right 2))]
         ;take 1M & 1C from left
         [(and (= n 5) (>= m_left 1) (>= c_left 1))
          (list (- m_left 1) (- c_left 1) (+ boat 1) (+ m_right 1) (+ c_right 1))]
         ;else none
         [else '()]
         )]
      
      [(equal? boat 1) ; boat 1 = right
       (cond
         ;take 1M from right
         [(and (= n 1) (>= m_right 1))
          (list (+ m_left 1) c_left (- boat 1) (- m_right 1) c_right)]
         ;take 2M from right
         [(and (= n 2) (>= m_right 2))
          (list (+ m_left 2) c_left (- boat 1) (- m_right 2) c_right)]
         ;take 1C from right
         [(and (= n 3) (>= c_right 1))
          (list m_left (+ c_left 1) (- boat 1) m_right (- c_right 1))]
         ;take 2C from right
         [(and (= n 4) (>= c_right 2))
          (list m_left (+ c_left 2) (- boat 1) m_right (- c_right 2))]
         ;take 1M & 1C from right
         [(and (= n 5) (>= m_right 1) (>= c_right 1))
          (list (+ m_left 1) (+ c_left 1) (- boat 1) (- m_right 1) (- c_right 1))]
         ;else none
         [else '()]
         )]
      )
      )))

;;-----------------------------------------------
;;
;; Provide a function, given a partial path, that will determine if the goal state 
;; has been reached.
;;

(define goal?
  (lambda (finish) ;called initially in 'mission-cannibal-bfs' to store goal state
    (lambda (x) ;called in generalized-search to check a given path
      (equal? finish (car x)) ))) ;check particular (top) state from path is goal state

;helper function to reset bool value
(define valid-state?
  (lambda (state)
    (valid-state?-helper state #t)
    ))

;check if more Cannibals than Missionaries on either side
(define valid-state?-helper
  (lambda (state bool) ;given state to check, and bool variable to return
    (display "checking: ")
    (display state)
    (newline)
    ;(cond
      (when (and (>= (car (cdr state)) 1) (>= (car state) 1)) ;when (C_left >=1 AND M_left >=1) check left side 
       (set! bool (not (> (car (cdr state)) (car state)) ))
       ) ;true/false "C_left not > M_left"
      (when (and (>= (car (cdr (cdr (cdr (cdr state))))) 1) (>= (car (cdr (cdr (cdr state)))) 1) (equal? bool #t)) ;when (C_right >=1 AND M_right >=1) check right side
       (set! bool (not (> (car (cdr (cdr (cdr (cdr state))))) (car (cdr (cdr (cdr state)))))))
        ) ;true/false "C_right not > M_right"
       
    bool ;return condition.. default #true
    ))

;initial setup for generalized-search with DFS
(define mission-cannibal-dfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) append (list(list start)) '() 0) ))

;initial setup for generalized-search with BFS
(define mission-cannibal-bfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) (appendToEnd) (list(list start)) '() 0) ))


;;------------------ Utility Functions --------------------
;append to end -- used for BFS
(define appendToEnd
  (lambda ()
	(lambda (newStuff oldStuff)
      (reverse (append newStuff (reverse oldStuff))))))


;check if element a, in list lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f) ;a not in list
      (else (or (equal? (car lat) a)
                (member? a (cdr lat))))
      )))


;specify what type of search to call
(mission-cannibal-bfs ss gg)
;(mission-cannibal-dfs ss gg)


