#lang scheme/load

;author: Kollen Gruizenga
;8queens.rkt
; 8 Queens Problem - AI Project No. 2


; Program composes a "path-container" which is a list of "paths".
; Each "path" is a list of states in order
; When taking "car path" aka "caar path-container", you get the
; first state from the first path. Once done, it will display the first found possible
; path to reach the goal state using the specified search method (BFS or DFS).
;
;Goal is to traverse the possible paths to fill all of the columns/rows on the Chess board with a Queen,
;such that no Queen intersects with another Queen horizontally, vertically, or diagonally.
;
;
; In any given transition, the top state will try to be extended to each of
; its possible next-steps, and the search algorithm will check as it moves
; down a step whether that step is valid (does the Queen it placed conflict with any existing Queens?)

; State is represented as a list of columns being indicated by the index of the list, and
; the value at that index is the row. If the value is 0, that means there is no Queen placed in any
; rows for that column (there is no Queen in that column). 

;Start state
(define ss '(0 0 0 0 0 0 0 0)) ; no queens placed in any of the columns


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

      ((not (valid-state? (caar path-container))) ;if Queen's conflict, meaning in same row or diagonal as another Queen
                                (display "reject path - Queen's conflict")
                                (display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      ((finish (car path-container)) ;if there is 8 Queens placed and none conflict, then done
				(display nodeCnt)
                                (display " nodes examined")
       				(newline)
                                (display (reverse (car path-container)))
       				(newline)
       				(display ""))
            
      ((null? (caar path-container)) ;if an individual state is empty list () -- meaning col1 thru 8 have elements, or that there are 8 queens existing
                                (display "empty list " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed nodeCnt)) ;don't count node as visited

      ((member? (caar path-container) closed) ;if already in visited states path
       				(display "reject path " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      (else
       (display "Extending path ")
            (display (reverse (car path-container)))
            (newline)
            (generalized-search finish path-Extender extend-method ; vvv - reversed so last element & everything preluding
                                (extend-method (path-Extender (car path-container)) (cdr path-container)) ;path-container .. extend(newStuff oldStuff)
                                (cons (caar path-container) closed) ;closed = "visited states/nodes"
								(+ 1 nodeCnt)) ;nodeCnt
            ))))

;------------------------------------------------------------------------------------
;; Functions that, when given an individual path, represent the next possible steps
;; in moving from that current working path (each of the states move options, or empty '() if not possible)

(define extend-paths
  (lambda () ;from initial 8-queen-dfs call, to be passed to generalized-search
    (lambda (init_path)
      (get-new-paths init_path (get-list-next-steps (car init_path)) ))))

;returns list of new paths with the various added next steps
(define get-new-paths 
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
    (set! compLst (append compLst (list (gen-step state 1 1))))
    (set! compLst (append compLst (list (gen-step state 2 1))))
    (set! compLst (append compLst (list (gen-step state 3 1))))
    (set! compLst (append compLst (list (gen-step state 4 1))))
    (set! compLst (append compLst (list (gen-step state 5 1))))
    (set! compLst (append compLst (list (gen-step state 6 1))))
    (set! compLst (append compLst (list (gen-step state 7 1))))
    (set! compLst (append compLst (list (gen-step state 8 1))))
    compLst))

;//using the next column (like knowing rule of going to other side when boat is on one side),
;generate a location for queen placement in a row, ie: ((1,2)) --> ((1,2), (2,1)) or ((1,2), (2,2)) .. etc.
(define gen-step
  (lambda (lst n colVal)
    (let ((col1 (car lst))                                           ;col1 value
          (col2 (car (cdr lst)))                                     ;col2 value
          (col3 (car (cdr (cdr lst))))                               ;col3 value
          (col4 (car (cdr (cdr (cdr lst)))))                         ;col4 value
          (col5 (car (cdr (cdr (cdr (cdr lst))))))                   ;col5 value
          (col6 (car (cdr (cdr (cdr (cdr (cdr lst)))))))             ;col6 value
          (col7 (car (cdr (cdr (cdr (cdr (cdr (cdr lst))))))))       ;col7 value
          (col8 (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr lst))))))))));col8 value
    (cond
      [(> colVal 8) '()]
      [(not(equal?(element-at-col colVal lst) 0))
       (gen-step lst n (+ colVal 1))]
      [else ;generate the appropriate next step based on column
       (cond
         [(= colVal 1)
          (list n col2 col3 col4 col5 col6 col7 col8)]
         [(= colVal 2)
          (list col1 n col3 col4 col5 col6 col7 col8)]
         [(= colVal 3)
          (list col1 col2 n col4 col5 col6 col7 col8)]
         [(= colVal 4)
          (list col1 col2 col3 n col5 col6 col7 col8)]
         [(= colVal 5)
          (list col1 col2 col3 col4 n col6 col7 col8)]
         [(= colVal 6)
          (list col1 col2 col3 col4 col5 n col7 col8)]
         [(= colVal 7)
          (list col1 col2 col3 col4 col5 col6 n col8)]
         [(= colVal 8)
          (list col1 col2 col3 col4 col5 col6 col7 n)]
         [else '()] ;else none
         )]
      )
      )))



;;-----------------------------------------------
;;
;; Provide a function, given a partial path, that will determine if the goal state 
;; has been reached.
;;

;//Check if all 8 queens placed, and if valid?
(define goal?
  (lambda (x) ;called in generalized-search to check a given path
    (valid-state? (car x)) (not(eq? (element-at-col 8 (car x)) 0)) )) ;check particular (top) state from path is goal state


;return True if valid state, False if queens have conflicts
(define valid-state?
  (lambda (state)
    (cond
      [(get-conflicts state) #f]
      [else #t]
      )))

;helper func to reset colCnt
(define get-conflicts
  (lambda (state)
    (or ;compare each particular Queen to others for conflicts
     (get-conflicts-helper state state 1 1)
     (get-conflicts-helper state state 2 1)
     (get-conflicts-helper state state 3 1)
     (get-conflicts-helper state state 4 1)
     (get-conflicts-helper state state 5 1)
     (get-conflicts-helper state state 6 1)
     (get-conflicts-helper state state 7 1)
     (get-conflicts-helper state state 8 1)
    )))

;determine whether there are any conflicts between Queens
(define get-conflicts-helper
  (lambda (state nstate colCnt colCnt2) ;st st 1 1 .. st st 1 2 .. st st 1 1
    (cond
      ;check, if no next queen then end
      [(> colCnt2 8) #f]
      ;if Outer is not a queen
      [(equal? (element-at-col colCnt state) 0) #f]
      ;if Inner is not a queen
      [(equal? (element-at-col colCnt2 state) 0) #f]
      ;if comparing Queen to itself, skip to next Queen
      [(equal? colCnt2 colCnt)
       (get-conflicts-helper state (cdr nstate) colCnt (+ colCnt2 1))]
      
      [(diag-conflict? colCnt (element-at-col colCnt state) colCnt2 (element-at-col colCnt2 state)) #t]
      [(row-conflict? (element-at-col colCnt state) (element-at-col colCnt2 state)) #t]
      [else
       (get-conflicts-helper state (cdr nstate) colCnt (+ colCnt2 1))]
      )))

;check for diagonal conflicts of Queen's
(define diag-conflict?
  (lambda (c1 r1 c2 r2)
    (equal?
           (abs(- r1 r2))
           (abs(- c1 c2))
           )
    ))

;check for row conflicts of Queen's 
(define row-conflict?
  (lambda (r1 r2)
    (eq? (- r2 r1) 0)
    ))

;return next queen's row value, or () if no next queen
(define get-next-queen
  (lambda (state)
    (cond
      [(not (or (null? state) (null? (cdr state)) (equal? (car (cdr state)) '() ) (equal? (car (cdr state)) 0))) (car (cdr state))]
      [else '()]
      )))

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


;helper to reset the cnt var
(define element-at-col
  (lambda (colNum lst)
    (element-at-col-helper colNum lst 1)))
;retrieve the element (rowNum) for given columnNum
(define element-at-col-helper
  (lambda (colNum lst cnt)
    (cond
      [(eq? lst '()) 0];not in a column
      [(= colNum cnt) (car lst)]
      [else (element-at-col-helper colNum (cdr lst) (+ cnt 1))]
      )))

;helper to reset cnt var in move-to
(define move-to
  (lambda (index lst)
    (move-to-helper index lst 0)
    ))
;move to the new index
(define move-to-helper
  (lambda (index lst cnt)
    (cond
      [(= index cnt) lst]
      [else (move-to-helper index (cdr lst) (+ cnt 1)) ]
    )))

; -------------------------------


;initial setup for generalized-search with DFS
(define 8-queen-dfs
  (lambda (start)
    (generalized-search goal? (extend-paths) append (list(list start)) '() 0) ))

;initial setup for generalized-search with BFS
(define 8-queen-bfs
  (lambda (start)
    (generalized-search goal? (extend-paths) (appendToEnd) (list(list start)) '() 0) ))

;specify what type of search to call
(8-queen-dfs ss)
;(8-queen-bfs ss)

