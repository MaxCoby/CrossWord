#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)

(define-type (Optional A)
  (U (Some A) 'None))

(define-struct (Some A)
  ([value : A]))

(define-type Direction
  (U 'down 'across))

(define-struct LetterCell
  ([letter : Char]))

(define-struct NumberedCell
  ([letter : Char]
   [number : Integer]))

(define-type Cell
  (Optional (U LetterCell NumberedCell)))

(define-struct Clue
  ([number : Integer]
   [direction : Direction]
   [text : String]))

(define-struct CrosswordPuzzle
  ([num-cells-across : Integer]
   [num-cells-down   : Integer]
   [cells : (Listof Cell)]
   [clues : (Listof Clue)]
   [source : (Optional String)]))

(define-struct Click
  ([x : Integer]
   [y : Integer]))

(define-struct CrossWorld
  ([cell-size : Integer]
   [puzzle : CrosswordPuzzle]
   [last-click : (Optional Click)]
   [current-direction : (Optional Direction)]
   [current-cell-index : (Optional Integer)]
   [candidate-solution : (Listof (Optional Char))]
   [display-solution : Boolean]
   [check : Boolean]))

(define nyt-mini-nov-10-2020-clues
  (list
   (Clue 1 'across "Piece starting on the second of seventh rank, in chess")
   (Clue 5 'across "The \"O\" of B&O Railroad")
   (Clue 6 'across "Where a river meets the sea")
   (Clue 8 'across "LSD, by another name")
   (Clue 9 'across "What Pac-Man eats")
   (Clue 1 'down "Group of close friends and relatives, in 2020-speak")
   (Clue 2 'down "In front")
   (Clue 3 'down "Alt-rock band fronted by Jeff Tweedy")
   (Clue 4 'down "Cry in a game of tag")
   (Clue 7 'down "Fast-forwarded parts of podcasts")))

;; for testing purposes
(define nyt-mini-nov-10-2020-cells
  (list
   ;; row 1
   (Some (NumberedCell #\P 1))
   (Some (NumberedCell #\A 2))
   (Some (NumberedCell #\W 3))
   (Some (NumberedCell #\N 4))
   'None
   ;; row 2
   (Some (NumberedCell #\O 5))
   (Some (LetterCell #\H))
   (Some (LetterCell #\I))
   (Some (LetterCell #\O))
   'None
   ;; row 3
   (Some (NumberedCell #\D 6))
   (Some (LetterCell #\E))
   (Some (LetterCell #\L))
   (Some (LetterCell #\T))
   (Some (NumberedCell #\A 7))
   ;; row 4
   'None
   (Some (NumberedCell #\A 8))
   (Some (LetterCell #\C))
   (Some (LetterCell #\I))
   (Some (LetterCell #\D))
   ;; row 5
   'None
   (Some (NumberedCell #\D 9))
   (Some (LetterCell #\O))
   (Some (LetterCell #\T))
   (Some (LetterCell #\S))))

(define nyt-mini-nov-10-2020
  (CrosswordPuzzle
   5
   5
   nyt-mini-nov-10-2020-cells
   nyt-mini-nov-10-2020-clues
   (Some "https://www.nytimes.com/crosswords/game/mini, Nov 10 2020")))

(define mini-rectangle
  (CrosswordPuzzle
   5
   3
   (list
    ;; row 1
    (Some (NumberedCell #\K 1))
    (Some (LetterCell #\A))
    (Some (NumberedCell #\R 2))
    (Some (LetterCell #\A))
    (Some (NumberedCell #\T 3))
    ;; row 2
    (Some (LetterCell #\F))
    'None
    (Some (LetterCell #\N))
    'None
    (Some (LetterCell #\O))
    ;; row 3
    (Some (NumberedCell #\C 4))
    (Some (LetterCell #\A))
    (Some (LetterCell #\R))
    (Some (LetterCell #\A))
    (Some (LetterCell #\T)))
   (list
    (Clue 1 'across "Gold purity measure")
    (Clue 4 'across "Gem size measure")
    (Clue 1 'down "Col.'s delight?")
    (Clue 2 'down "Really at ease?")
    (Clue 3 'down "Tater morphology"))
   (Some "Adam Shaw Nov 15 2020"))) 

(define minimal-puzzle
  (CrosswordPuzzle 1
                   1
                   (list (Some (NumberedCell #\A 1)))
                   (list (Clue 1 'across "It's the letter A")
                         (Clue 1 'down "It's still the letter A"))
                   'None))

(: take : All (A) Integer (Listof A) -> (Listof A))
;; take n items from the front of a list
(define (take n xs)
  (match* (n xs)
    [(0 _) '()]
    [(_ '()) '()]
    [(_ (cons f r)) (cons f (take (- n 1) r))]))
(check-expect (take 0 '(1 2 3 4 5)) '())
(check-expect (take 3 '(1 2 3 4 5)) '(1 2 3))
(check-expect (take 5 '(1 2 3 4 5)) '(1 2 3 4 5))

(: drop : All (A) Integer (Listof A) -> (Listof A))
;; drop n items from the front of a list
(define (drop n xs)
  (match* (n xs)
    [(_ '()) '()]
    [(0 (cons f r)) (cons f r)]
    [(_ (cons f r)) (drop (- n 1) r)]))
(check-expect (drop 0 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (drop 3 '(1 2 3 4 5)) '(4 5))
(check-expect (drop 5 '(1 2 3 4 5)) '())

(: draw-row-solution : (Listof Cell) Integer -> Image)
;; produce an image that represents a given row of a crossword puzzle
;; with the solution displayed
;; given a list of cells to represent a row and the cell size
(define (draw-row-solution cells size)
  (match cells
    ['() empty-image]
    [_ (match (first cells)
         ['None (beside
                 (square size 'solid 'black)
                 (draw-row-solution (drop 1 cells) size))]
         [(Some value)
          (match value
            [(LetterCell letter)
             (beside
              (overlay
               (text (string letter) 12 'black)
               (square size 'outline 'black))
              (draw-row-solution (drop 1 cells) size))]
            [(NumberedCell letter number)
             (beside
              (overlay
               (text (string letter) 12 'black) 
               (underlay/align/offset
                "right" "top"
                (square size 'outline 'black)
                (- 8 size) 3
                (text (number->string number) 8 'black)))
              (draw-row-solution (drop 1 cells) size))])])]))
(draw-row-solution (list
                    (Some (NumberedCell #\D 6))
                    (Some (LetterCell #\E))
                    (Some (LetterCell #\L))
                    (Some (LetterCell #\T))
                    (Some (NumberedCell #\A 7)))
                   30)
(draw-row-solution (list
                    (Some (LetterCell #\F))
                    'None
                    (Some (LetterCell #\N))
                    'None
                    (Some (LetterCell #\O)))
                   50)

(: draw-row : (Listof Cell) Integer (Listof (Optional Char)) -> Image)
;; produce an image that represents a given row of a crossword puzzle
;; without the solution displayed but with user inputs displayed
;; given a list of cells to represent a row and the cell size
(define (draw-row cells size cand)
  (match cells
    ['() empty-image]
    [_ (match (first cells)
         ['None (beside
                 (square size 'solid 'black)
                 (draw-row (drop 1 cells) size (drop 1 cand)))]
         [(Some value)
          (match value
            [(LetterCell letter)
             (beside
              (match (list-ref cand 0)
                ['None (square size 'outline 'black)]
                [(Some char) (overlay
                              (text (string char) 12 'black)
                              (square size 'outline 'black))])
              (draw-row (drop 1 cells) size (drop 1 cand)))]
            [(NumberedCell letter number)
             (beside
              (match (list-ref cand 0)
                ['None (underlay/align/offset
                        "right" "top"
                        (square size 'outline 'black)
                        (- 8 size) 3
                        (text (number->string number) 8 'black))]
                [(Some char) (overlay
                              (text (string char) 12 'black)
                              (underlay/align/offset
                               "right" "top"
                               (square size 'outline 'black)
                               (- 8 size) 3
                               (text (number->string number) 8 'black)))])
              (draw-row (drop 1 cells) size (drop 1 cand)))])])]))
(draw-row (list
           (Some (NumberedCell #\D 6))
           (Some (LetterCell #\E))
           (Some (LetterCell #\L))
           (Some (LetterCell #\T))
           (Some (NumberedCell #\A 7)))
          30
          (list (Some #\A) 'None (Some #\B) (Some #\C) 'None))
(draw-row (list
           (Some (LetterCell #\F))
           'None
           (Some (LetterCell #\N))
           'None
           (Some (LetterCell #\O)))
          50
          (list 'None 'None 'None 'None (Some #\D)))

(: draw-row-check : (Listof Cell) Integer (Listof (Optional Char)) -> Image)
;; produce an image that represents a given row of a crossword puzzle
;; without the solution displayed but with user inputs displayed
;; and a green highlight to indiate a correct input or a
;; red highlighht to indicate an incorrect input
;; given a list of cells to represent a row and the cell size
(define (draw-row-check cells size cand)
  (match cells
    ['() empty-image]
    [_ (match (first cells)
         ['None (beside
                 (square size 'solid 'black)
                 (draw-row-check (drop 1 cells) size (drop 1 cand)))]
         [(Some value)
          (match value
            [(LetterCell letter)
             (beside
              (match (list-ref cand 0)
                ['None (square size 'outline 'black)]
                [(Some char) (if (char=? char letter)
                                 (overlay
                                  (text (string char) 12 'black)
                                  (square size 100 'green)
                                  (square size 'outline 'black))
                                 (overlay
                                  (text (string char) 12 'black)
                                  (square size 100 'red)                    
                                  (square size 'outline 'black)))])
              (draw-row-check (drop 1 cells) size (drop 1 cand)))]
            [(NumberedCell letter number)
             (beside
              (match (list-ref cand 0)
                ['None (underlay/align/offset
                        "right" "top"
                        (square size 'outline 'black)
                        (- 8 size) 3
                        (text (number->string number) 8 'black))]
                [(Some char) (if (char=? char letter)
                                 (overlay
                                  (text (string char) 12 'black)
                                  (square size 100 'green)
                                  (underlay/align/offset
                                   "right" "top"
                                   (square size 'outline 'black)
                                   (- 8 size) 3
                                   (text (number->string number) 8 'black)))
                                 (overlay
                                  (text (string char) 12 'black)
                                  (square size 100 'red)
                                  (underlay/align/offset
                                   "right" "top"
                                   (square size 'outline 'black)
                                   (- 8 size) 3
                                   (text (number->string number) 8 'black))))])
              (draw-row-check (drop 1 cells) size (drop 1 cand)))])])]))
(draw-row-check (list
                 (Some (NumberedCell #\D 6))
                 (Some (LetterCell #\E))
                 (Some (LetterCell #\L))
                 (Some (LetterCell #\T))
                 (Some (NumberedCell #\A 7)))
                30
                (list (Some #\A) 'None (Some #\L) (Some #\C) 'None))
(draw-row-check (list
                 (Some (LetterCell #\F))
                 'None
                 (Some (LetterCell #\N))
                 'None
                 (Some (LetterCell #\O)))
                50
                (list 'None 'None 'None 'None (Some #\O)))
                             
(: draw-puzzle : CrosswordPuzzle Integer Boolean Boolean
   (Listof (Optional Char)) -> Image)
;; produce an image that represents an entire crossword puzzle
;; with user inputs displayed
;; given boolean to display entire solution (#t solution, #f no solution) and
;; another boolean to display checking highlights (#t checking, #f no checking)
(define (draw-puzzle puzzle size display check cand)
  (match puzzle
    [(CrosswordPuzzle col row cells clues source)
     (match row
       [0 empty-image]
       [_ (local
            {(define next-puzzle (CrosswordPuzzle col
                                                  (- row 1)
                                                  (drop col cells)
                                                  clues
                                                  source))}
            (if display
                (above
                 (draw-row-solution (take col cells) size)
                 (draw-puzzle next-puzzle size display check cand))
                (if check
                    (above
                     (draw-row-check (take col cells) size (take col cand))
                     (draw-puzzle
                      next-puzzle size display check (drop col cand)))
                    (above
                     (draw-row (take col cells) size (take col cand))
                     (draw-puzzle
                      next-puzzle size display check (drop col cand))))))])]))
             
(draw-puzzle nyt-mini-nov-10-2020 30 #f #f (make-list 25 'None))
(draw-puzzle mini-rectangle 30 #t #t (make-list 15 'None))
(draw-puzzle mini-rectangle 30 #f #t
             (list 'None (Some #\A) 'None (Some #\B) 'None
                   (Some #\C) 'None 'None 'None (Some #\D)
                   'None 'None (Some #\F) 'None 'None))
(draw-puzzle minimal-puzzle 50 #f #f (make-list 1 'None))

(: draw-across-clues : (Listof Clue) (Optional Integer) (Optional Direction)
   -> Image)
;; produce an image that shows the across clues for a given crossword puzzle
;; highlight the current across clue (if applicable)
;; that is being solved for based on user's click
;; given current clue to be checked and current direction
(define (draw-across-clues clues current dir)
  (match clues
    ['() empty-image]
    [(cons f r)
     (match f
       [(Clue number direction hint)
        (match direction
          ['across
           (local
             {(define actual-clue
                (text (string-append (number->string number) ". " hint)
                      12
                      'black))}
             (above/align
              "left"
              (match* (current dir)
                [('None _) actual-clue]
                [((Some value) (Some d))
                 (if (and (= number value) (symbol=? direction d))
                     (overlay (rectangle (image-width actual-clue)
                                         (image-height actual-clue)
                                         100
                                         'yellow)
                              actual-clue)
                     actual-clue)])
              (square 3 'solid 'white)
              (draw-across-clues r current dir)))]
          [_ (draw-across-clues r current dir)])])]))
(draw-across-clues nyt-mini-nov-10-2020-clues 'None 'None)
(draw-across-clues nyt-mini-nov-10-2020-clues (Some 1) (Some 'across))
(draw-across-clues nyt-mini-nov-10-2020-clues (Some 1) (Some 'down))
(draw-across-clues nyt-mini-nov-10-2020-clues (Some 6) (Some 'across))
(draw-across-clues nyt-mini-nov-10-2020-clues (Some 6) (Some 'down))

(: draw-down-clues : (Listof Clue) (Optional Integer) (Optional Direction)
   -> Image)
;; produce an image that shows the down clues for a given crossword puzzle
;; highlight the current down clue (if applicable)
;; that is being solved for based on user's click
;; given current clue to be checked and current direction
(define (draw-down-clues clues current dir)
  (match clues
    ['() empty-image]
    [(cons f r)
     (match f
       [(Clue number direction hint)
        (match direction
          ['down
           (local
             {(define actual-clue
                (text (string-append (number->string number) ". " hint)
                      12
                      'black))}
             (above/align
              "left"
              (match* (current dir)
                [('None _) actual-clue]
                [((Some value) (Some d))
                 (if (and (= number value) (symbol=? direction d))
                     (overlay (rectangle (image-width actual-clue)
                                         (image-height actual-clue)
                                         100
                                         'yellow)
                              actual-clue)
                     actual-clue)])
              (square 3 'solid 'white)
              (draw-down-clues r current dir)))]
          [_ (draw-down-clues r current dir)])])]))
(draw-down-clues nyt-mini-nov-10-2020-clues 'None 'None)
(draw-down-clues nyt-mini-nov-10-2020-clues (Some 1) (Some 'across))
(draw-down-clues nyt-mini-nov-10-2020-clues (Some 1) (Some 'down))
(draw-down-clues nyt-mini-nov-10-2020-clues (Some 3) (Some 'across))
(draw-down-clues nyt-mini-nov-10-2020-clues (Some 3) (Some 'down))

(: draw-clues : (Listof Clue) (Optional Integer) (Optional Direction) -> Image)
;; produce an image that shows the across and down clues with labels
;; for a given crossword puzzle
;; highlight the current across or down clue (if applicable)
;; that is being solved for based on user's click
;; given current clue to be checked and current direction
(define (draw-clues clues current dir)
  (if (> (length clues) 50)
      (beside/align
       "top"
       (above/align
        "left"
        (text "ACROSS" 20 'black)
        (square 5 'solid 'white)
        (draw-across-clues clues current dir))
       (square 10 'solid 'white)
       (above/align
        "left"
         (text "DOWN" 20 'black)
         (square 5 'solid 'white)
         (draw-down-clues clues current dir)))
      (above/align
       "left"
       (text "ACROSS" 20 'black)
       (square 5 'solid 'white)
       (draw-across-clues clues current dir)
       (square 10 'solid 'white)
       (text "DOWN" 20 'black)
       (square 5 'solid 'white)
       (draw-down-clues clues current dir))))
(draw-clues nyt-mini-nov-10-2020-clues (Some 9) (Some 'across))
(draw-clues (CrosswordPuzzle-clues mini-rectangle) (Some 3) (Some 'down))
(draw-clues (CrosswordPuzzle-clues minimal-puzzle) (Some 1) (Some 'down))

(: top-left : CrossWorld Integer Integer Integer -> (Listof Click))
;; compute the top left corners of all the squares in the CrossWorld
;; when called... counter must be 1, col counter and row counter must be 0
(define (top-left world counter col-counter row-counter)
  (match world
    [(CrossWorld size puzzle _ _ _ _ _ _)
     (local
       {(define col (CrosswordPuzzle-num-cells-across puzzle))
        (define row (CrosswordPuzzle-num-cells-down puzzle))}
       (if (= (- counter 1) (* col row))
           '()
           (cons (Click (+ (* col-counter size))
                        (+ (* row-counter size)))
                 (if (= (modulo counter col) 0)
                     (top-left world
                               (+ counter 1)
                               0
                               (+ row-counter 1))
                     (top-left world
                               (+ counter 1)
                               (+ col-counter 1)
                               row-counter)))))]))
(check-expect
 (top-left
  (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
 (list (Click 0 0) (Click 30 0) (Click 60 0) (Click 90 0)
       (Click 120 0) (Click 0 30) (Click 30 30)
       (Click 60 30) (Click 90 30) (Click 120 30)
       (Click 0 60) (Click 30 60) (Click 60 60)
       (Click 90 60) (Click 120 60) (Click 0 90)
       (Click 30 90) (Click 60 90) (Click 90 90)
       (Click 120 90) (Click 0 120) (Click 30 120)
       (Click 60 120) (Click 90 120) (Click 120 120)))
(check-expect
 (top-left
  (CrossWorld 20 mini-rectangle 'None 'None 'None '() #f #f) 1 0 0)
 (list (Click 0 0) (Click 20 0) (Click 40 0) (Click 60 0) (Click 80 0)
       (Click 0 20) (Click 20 20) (Click 40 20) (Click 60 20) (Click 80 20)
       (Click 0 40) (Click 20 40) (Click 40 40) (Click 60 40) (Click 80 40)))
(check-expect
 (top-left
  (CrossWorld 30 minimal-puzzle 'None 'None 'None '() #f #f) 1 0 0)
 (list (Click 0 0)))

(: in-a-square? : Integer Click (Listof Click) Integer -> (U Click 'Not))
;; if a square was clicked on, return the top left corner of that square
;; if no square was clicked on, return 'Not
;; when called... list index must be 0
(define (in-a-square? size click cs list-index)
  (match cs
    ['() 'Not]
    [_
     (if (= list-index (length cs))
         'Not        
         (local
           {(define top-left (list-ref cs list-index))}
           (if (and (<= (- (Click-x click) (Click-x top-left)) size)
                    (<= (- (Click-y click) (Click-y top-left)) size))
               top-left
               (in-a-square? size click cs (+ list-index 1)))))]))
(check-expect
 (in-a-square?
  30
  (Click 15 15)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Click 0 0))
(check-expect
 (in-a-square?
  30
  (Click 135 135)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Click 120 120))
(check-expect
 (in-a-square?
  15
  (Click 35 55)
  (top-left
   (CrossWorld 15 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Click 30 45))
(check-expect
 (in-a-square?
  30
  (Click 151 50)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 'Not)
(check-expect
 (in-a-square?
  30
  (Click 50 151)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 'Not)

(: square-index : Integer Click (Listof Click) Integer -> (Optional Integer))
;; if a square was clicked on, return the list-index of that square
;; if no square was clicked on, return 'None
;; when called... list index must be 0
(define (square-index size click cs list-index)
  (match cs
    ['() 'None]
    [_
     (if (= list-index (length cs))
         'None        
         (local
           {(define top-left (list-ref cs list-index))}
           (if (and (<= (- (Click-x click) (Click-x top-left)) size)
                    (<= (- (Click-y click) (Click-y top-left)) size))
               (Some list-index)
               (square-index size click cs (+ list-index 1)))))]))
(check-expect
 (square-index
  30
  (Click 15 15)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Some 0))
(check-expect
 (square-index
  30
  (Click 135 135)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Some 24))
(check-expect
 (square-index
  15
  (Click 35 55)
  (top-left
   (CrossWorld 15 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 (Some 17))
(check-expect
 (square-index
  30
  (Click 151 50)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 'None)
(check-expect
 (square-index
  30
  (Click 50 151)
  (top-left
   (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f) 1 0 0)
  0)
 'None)

(: replace-at : All (A) Integer A (Listof (Optional A))
   -> (Listof (Optional A)))
;; replace the item at the given position
;; position counting starts at 0
(define (replace-at i x xs)
  (match xs
    [(cons f r)
     (match i
       [0 (cons (Some x) r)]
       [_ (cons f (replace-at (- i 1) x r))])]))
(check-expect (replace-at
               2
               #\Z
               (list 'None (Some #\A) 'None (Some #\B) 'None))
              (list 'None (Some #\A) (Some #\Z) (Some #\B) 'None))
(check-expect (replace-at
               3
               #\Z
               (list 'None (Some #\A) 'None (Some #\B) 'None))
              (list 'None (Some #\A) 'None (Some #\Z) 'None))
(check-expect (replace-at
               4
               #\Z
               (list 'None (Some #\A) 'None (Some #\B) 'None))
              (list 'None (Some #\A) 'None (Some #\B) (Some #\Z)))

(: draw-arrow-across : Integer -> Image)
;; produce an image of an across arrow that fits into a cell of a
;; given cell size
(define (draw-arrow-across size)
  (scale
   (/ size 28)
   (beside
    (rectangle 15 6 100 'lightblue)
    (rotate 30 (triangle 15 100 'lightblue)))))
(draw-arrow-across 20)
(check-expect (image-width (draw-arrow-across 20)) 20)
(draw-arrow-across 50)
(check-expect (image-width (draw-arrow-across 50)) 50)

(: draw-arrow-down : Integer -> Image)
;; produce an image of a down arrow that fits into a cell of a
;; given cell size
(define (draw-arrow-down size)
  (scale
   (/ size 28)
   (above
    (rectangle 6 15 100 'lightblue)
    (rotate 180 (triangle 15 100 'lightblue)))))
(draw-arrow-down 20)
(check-expect (image-height (draw-arrow-down 20)) 20)
(draw-arrow-down 50)
(check-expect (image-height (draw-arrow-down 50)) 50)

(: find-row : (Listof Cell) Integer Integer -> (Listof Cell))
;; return the row that the given cell index is contained in
;; given entire list of cells, target index, and number of columns
;; in the puzzle
(define (find-row cells index col)
  (match cells
    ['() '()]
    [_ 
     (local
       {(define row (take col cells))}
       (if (< index col) 
           row
           (find-row (drop col cells) (- index col) col)))]))
(check-expect
 (find-row nyt-mini-nov-10-2020-cells 9 5)
 (list (Some (NumberedCell #\O 5)) (Some (LetterCell #\H))
       (Some (LetterCell #\I)) (Some (LetterCell #\O)) 'None))
(check-expect
 (find-row (CrosswordPuzzle-cells mini-rectangle) 10 5)
 (list (Some (NumberedCell #\C 4)) (Some (LetterCell #\A))
       (Some (LetterCell #\R)) (Some (LetterCell #\A)) (Some (LetterCell #\T))))
(check-expect
 (find-row (CrosswordPuzzle-cells minimal-puzzle) 0 1)
 (list (Some (NumberedCell #\A 1))))
 
(: get-row-index : Integer Integer -> Integer)
;; convert the given cell index within the entire puzzle to
;; the cell index within just its row
;; given the number of columns in the puzzle
(define (get-row-index index col)
  (if (< index col)
      index
      (get-row-index (- index col) col)))
(check-expect (get-row-index 9 5) 4)
(check-expect (get-row-index 11 5) 1)
(check-expect (get-row-index 0 1) 0)

(: reduce-col : Integer Integer -> Integer)
;; given the cell index within the entire puzzle,
;; return the cell index of the uppermost cell that
;; is in the same column as the original cell index
;; given the number of columns in the puzzle
(define (reduce-col index col)
  (if (< index col)
      index
      (reduce-col (- index col) col)))
(check-expect (reduce-col 12 5) 2)
(check-expect (reduce-col 4 2) 0)
(check-expect (reduce-col 16 3) 1)

(: find-col : (Listof Cell) Integer Integer -> (Listof Cell))
;; return the column that the given cell index is contained in
;; given entire list of cells, target index, and number of columns
;; in the puzzle
(define (find-col cells index col)
  (match* (cells index)
    [('() _) '()]
    [(_ i)
     (local
       {(define new-index (reduce-col i col))
        (define max-index (- (length cells) 1))
        (: find-col-help : (Listof Cell) Integer Integer Integer
           -> (Listof Cell))
        (define (find-col-help cells index max col)
          (if (> index max)
              '()
              (cons (list-ref cells index)
                    (find-col-help cells (+ index col) max col))))}
       (find-col-help cells new-index max-index col))]))
(check-expect
 (find-col nyt-mini-nov-10-2020-cells 9 5)
 (list 'None 'None (Some (NumberedCell #\A 7)) 
       (Some (LetterCell #\D)) (Some (LetterCell #\S))))
(check-expect
 (find-col (CrosswordPuzzle-cells mini-rectangle) 10 5)
 (list (Some (NumberedCell #\K 1)) (Some (LetterCell #\F))
       (Some (NumberedCell #\C 4))))
(check-expect
 (find-col (CrosswordPuzzle-cells minimal-puzzle) 0 1)
 (list (Some (NumberedCell #\A 1))))

(: get-col-index : Integer Integer -> Integer)
;; convert the given cell index within the entire puzzle to
;; the cell index within just its column
;; given the number of columns in the puzzle
(define (get-col-index index col)
  (floor (/ index col)))
(check-expect (get-col-index 9 5) 1)
(check-expect (get-col-index 11 5) 2)
(check-expect (get-col-index 0 1) 0)

(: is-clue? : (Listof Cell) Integer -> (U Boolean Integer))
;; determine if the given cell index is part of a given across or down clue
;; return the number of the NumberedCell for whichever clue the cell index
;; is a part of or false if the cell index is not a part of the given clue
(define (is-clue? cells index)
  (if (< index 0)
      #f
      (match (list-ref cells index)
        ['None #f]
        [(Some (NumberedCell _ number)) (if
                                         (= index 0)
                                         number
                                         (match (list-ref cells (- index 1))
                                           ['None number]
                                           [_ (is-clue? cells (- index 1))]))]
        [(Some (LetterCell _)) (is-clue? cells (- index 1))])))
(check-expect (is-clue?
               (list (Some (NumberedCell #\D 6))
                     (Some (LetterCell #\E))
                     (Some (LetterCell #\L))
                     (Some (LetterCell #\T))
                     (Some (NumberedCell #\A 7)))
               4)
              6)
(check-expect (is-clue?
               (list 'None
                     (Some (NumberedCell #\D 9))
                     (Some (LetterCell #\O))
                     (Some (LetterCell #\T))
                     (Some (LetterCell #\S)))
               0)
              #f)
(check-expect (is-clue?
               (list (Some (NumberedCell #\K 1))
                     (Some (LetterCell #\F))
                     (Some (NumberedCell #\C 4)))
               2)
              1)
    
(: find-clue : (Listof Cell) Integer (Optional Integer) (Optional Direction)
   -> (Optional Integer))
;; determine if the given cell index is part of any across or down clue
;; return the number of the NumberedCell for whichever clue the cell index
;; is a part of or false if the cell index is not a part of any clue
;; also given number of columns in the puzzle, current index,
;; and current direction based on user's click
(define (find-clue cells col index dir)
  (match* (index dir)
    [('None _) 'None]
    [((Some value) (Some direction))
     (local
       {(define row-with-cell (find-row cells value col))
        (define clue-across? (is-clue? row-with-cell (get-row-index value col)))
        (define col-with-cell (find-col cells value col))
        (define clue-down? (is-clue? col-with-cell (get-col-index value col)))}
       (match direction
         ['across
          (if (> (length row-with-cell) 0)
              (if (number? clue-across?)
                  (Some clue-across?)
                  'None)
              'None)]
         ['down
          (if (> (length col-with-cell) 0)
              (if (number? clue-down?)
                  (Some clue-down?)
                  'None)
              'None)]))]))
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 (Some 12) (Some 'across))
              (Some 6))
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 (Some 12) (Some 'down))
              (Some 3))
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 (Some 24) (Some 'across))
              (Some 9))
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 (Some 24) (Some 'down))
              (Some 7))
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 (Some 4) (Some 'across))
              'None)
(check-expect (find-clue nyt-mini-nov-10-2020-cells 5 'None 'None) 'None)

(: puzzle-source : String -> (Optional String))
;; get the source from a xwp file
(define (puzzle-source file)
  (local
    {(define puzzle-list (file->lines file))
     (define source (list-ref puzzle-list 0))}
    (if (char=? (string-ref source 0) #\s)
        (Some (substring source 7))
        'None)))
(check-expect (puzzle-source "nyt-mini-nov-27-2020.xwp")
              (Some "New York Times Mini, Nov 27, 2020"))
(check-expect (puzzle-source "nyt-jan-25-2016.xwp")
              (Some "New York Times, Monday Jan 25, 2016 by Ian Livengood"))

(: first-split (-> (Listof Char) Char (Listof Char)))
;; Produces the list of characters up until the first given Char
;; Source: lab4-include.rkt
(define (first-split str c)
  (match str
    ['() '()]
    [(cons head tail)
     (if (char=? head c)
         '()
         (cons head (first-split tail c)))]))
(check-expect (first-split (string->list "aa") #\,) (string->list "aa"))
(check-expect (first-split (string->list "aaa,bbbbb,b,b") #\,)
              (string->list "aaa"))
(check-expect (first-split (string->list "aaa/bbbbb,b,b") #\/)
              (string->list "aaa"))

(: rest-split (-> (Listof Char) Char (Listof Char)))
;; Produces the list of characters after the first given Char
;; Source: lab4-include.rkt
(define (rest-split str c)
  (match str
    ['() '()]
    [(cons head tail)
     (if (char=? head c)
         tail
         (rest-split tail c))]))
(check-expect (rest-split (string->list "aa") #\,) (string->list ""))
(check-expect (rest-split (string->list "aaa/bbbbb,b") #\/)
              (string->list "bbbbb,b"))

(: list-split (-> (Listof Char) Char (Listof (Listof Char))))
;; Creates a list of list of Chars, separated based on the given Char
;; Source: lab4-include.rkt
(define (list-split str c)
  (match str
    ['() '()]
    [_ (cons (first-split str c)
             (list-split (rest-split str c) c))]))
(check-expect (list-split (string->list "aa") #\,) (list (string->list "aa")))
(check-expect (list-split (string->list "aaa,bbbbb,b") #\,) 
              (list (string->list "aaa") 
                    (string->list "bbbbb") 
                    (string->list "b")))
(check-expect (list-split (string->list "aaa/bbbbb,b") #\/) 
              (list (string->list "aaa") (string->list "bbbbb,b")))

(: split-string (-> String Char (Listof String)))
;; Takes a string and splits it based on the given Char using list-split
;; Source: lab4-include.rkt
(define (split-string str c)
  (map list->string (list-split (string->list str) c)))
(check-expect (split-string "aa" #\,) (list "aa"))
(check-expect (split-string "aaa,bbbbb,b" #\,) 
              (list "aaa" "bbbbb" "b"))
(check-expect (split-string "aaa/bbbbb,b" #\/) 
              (list "aaa" "bbbbb,b"))

(: puzzle-col : String -> Integer)
;; get the num-cells-aross from a xwp file
(define (puzzle-col file)
  (local
    {(define puzzle-list (file->lines file))
     (define line (if (char=? (string-ref (list-ref puzzle-list 0) 0) #\s)
                      (list-ref puzzle-list 1)
                      (list-ref puzzle-list 0)))}
    (length (split-string line #\,))))
(check-expect (puzzle-col "nyt-mini-nov-27-2020.xwp") 5)
(check-expect (puzzle-col "nyt-jan-25-2016.xwp") 15)

(: is-puzzle-row? : String -> Boolean)
;; determine if a given line from a xwp file is formatted
;; as a row of the crossword puzzle 
(define (is-puzzle-row? line)
  (not (or (string-contains? line "source")
           (string-contains? line "across")
           (string-contains? line "down"))))
(check-expect (is-puzzle-row? "source:Blah") #f)
(check-expect (is-puzzle-row? "1across:Blah") #f)
(check-expect (is-puzzle-row? "1down:Blah") #f)
(check-expect (is-puzzle-row? "8.B,E,E,S,-") #t)

(: puzzle-row : String -> Integer)
;; get the num-cells-down from a xwp file
(define (puzzle-row file)
  (local
    {(define puzzle-list (file->lines file))}
    (length (filter is-puzzle-row? puzzle-list))))
(check-expect (puzzle-row "nyt-mini-nov-27-2020.xwp") 5)
(check-expect (puzzle-row "nyt-jan-25-2016.xwp") 15)

(: get-index-clues : (Listof String) Integer -> Integer)
;; determine the index of the line where clues first begin to appear
;; when called... counter must be 0
(define (get-index-clues lines counter)
  (if (or
       (string-contains? (list-ref lines 0) "across")
       (string-contains? (list-ref lines 0) "down"))
      counter
      (get-index-clues (drop 1 lines) (+ 1 counter))))
(check-expect (get-index-clues (file->lines "nyt-mini-nov-27-2020.xwp") 0) 6)
(check-expect (get-index-clues (file->lines "nyt-jan-25-2016.xwp") 0) 16)

(: get-puzzle-cells-lines : String -> (Listof String))
;; return a list of strings that contains only the lines formatted as crossword
;; puzzle rows from a xwp file
(define (get-puzzle-cells-lines file)
  (local
    {(define puzzle-list (file->lines file))}
    (match (puzzle-source file)
      ['None (take (get-index-clues puzzle-list 0) puzzle-list)]
      [_ (drop 1 (take (get-index-clues puzzle-list 0) puzzle-list))])))
(check-expect (get-puzzle-cells-lines "nyt-mini-nov-27-2020.xwp")
              '("-,1.F,2.O,3.I,4.L"
                "-,5.L,U,K,E"
                "6.J,A,N,E,T"
                "7.O,R,C,A,-"
                "8.B,E,E,S,-"))
(check-expect (get-puzzle-cells-lines "nyt-jan-25-2016.xwp")
              '("1.L,2.E,3.F,4.T,-,5.A,6.L,7.S,8.O,-,-,9.A,10.B,11.C,12.S"
                "13.I,R,I,S,-,14.N,O,K,I,15.A,-,16.T,O,O,L"
                "17.M,A,S,K,18.E,D,B,A,L,L,-,19.T,O,N,E"
                "20.A,S,H,-,21.A,R,E,-,22.S,T,23.A,I,N,E,D"
                "-,-,24.S,25.T,R,E,S,26.S,-,27.A,L,L,E,Y,S"
                "28.M,29.A,T,E,S,-,-,30.K,31.A,R,M,A,-,-,-"
                "32.A,V,I,D,-,33.T,34.H,E,N,B,A,-,35.F,36.O,37.E"
                "38.M,I,C,-,39.C,H,E,W,T,O,Y,-,40.U,V,A"
                "41.A,S,K,-,42.H,E,R,E,S,Y,-,43.O,N,E,S"
                "-,-,-,44.D,I,N,E,R,-,-,45.N,A,N,N,Y"
                "46.S,47.H,48.R,I,N,E,-,49.S,50.A,51.F,E,T,Y,-,-"
                "52.L,E,O,N,A,R,53.D,-,54.N,E,W,-,55.B,56.O,57.B"
                "58.E,A,V,E,-,59.V,E,60.L,V,E,T,61.R,O,P,E"
                "62.E,V,E,R,-,63.E,M,A,I,L,-,64.O,N,U,S"
                "65.P,E,S,O,-,-,66.O,W,L,S,-,67.W,E,S,T"))

(: get-puzzle-cells : (Listof String) -> (Listof String))
;; convert a list of lines formatted as rows into a list of strings that
;; contains every cell from the original list
(define (get-puzzle-cells lines)
  (match lines
    ['() '()]
    [(cons f r) (append (split-string f #\,) (get-puzzle-cells r))]))
(check-expect (get-puzzle-cells
               (get-puzzle-cells-lines "nyt-mini-nov-27-2020.xwp"))
              '("-" "1.F" "2.O" "3.I" "4.L" "-" "5.L" "U" "K" "E" "6.J" "A"
                    "N" "E" "T" "7.O" "R" "C" "A" "-" "8.B" "E" "E" "S" "-"))
(check-expect (get-puzzle-cells (get-puzzle-cells-lines "nyt-jan-25-2016.xwp"))
              '("1.L" "2.E" "3.F" "4.T" "-" "5.A" "6.L" "7.S" "8.O" "-" "-"
                      "9.A" "10.B" "11.C" "12.S" "13.I" "R" "I" "S" "-" "14.N"
                      "O" "K" "I" "15.A" "-" "16.T" "O" "O" "L" "17.M" "A" "S"
                      "K" "18.E" "D" "B" "A" "L" "L" "-" "19.T" "O" "N" "E"
                      "20.A" "S" "H" "-" "21.A" "R" "E" "-" "22.S" "T" "23.A"
                      "I" "N" "E" "D" "-" "-" "24.S" "25.T" "R" "E" "S" "26.S"
                      "-" "27.A" "L" "L" "E" "Y" "S" "28.M" "29.A" "T" "E" "S"
                      "-" "-" "30.K" "31.A" "R" "M" "A" "-" "-" "-" "32.A" "V"
                      "I" "D" "-" "33.T" "34.H" "E" "N" "B" "A" "-" "35.F"
                      "36.O" "37.E" "38.M" "I" "C" "-" "39.C" "H" "E" "W" "T"
                      "O" "Y" "-" "40.U" "V" "A" "41.A" "S" "K" "-" "42.H" "E"
                      "R" "E" "S" "Y" "-" "43.O" "N" "E" "S" "-" "-" "-" "44.D"
                      "I" "N" "E" "R" "-" "-" "45.N" "A" "N" "N" "Y" "46.S"
                      "47.H" "48.R" "I" "N" "E" "-" "49.S" "50.A" "51.F" "E" "T"
                      "Y" "-" "-" "52.L" "E" "O" "N" "A" "R" "53.D" "-" "54.N"
                      "E" "W" "-" "55.B" "56.O" "57.B" "58.E" "A" "V" "E" "-"
                      "59.V" "E" "60.L" "V" "E" "T" "61.R" "O" "P" "E" "62.E"
                      "V" "E" "R" "-" "63.E" "M" "A" "I" "L" "-" "64.O" "N" "U"
                      "S" "65.P" "E" "S" "O" "-" "-" "66.O" "W" "L" "S" "-"
                      "67.W" "E" "S" "T"))           

(: puzzle-cells : (Listof String) -> (Listof Cell))
;; get the cells from a xwp file
;; given a list of strings rather than the file name
(define (puzzle-cells cells)
  (match cells
    ['() '()]
    [(cons f r) (match (string-length f)
                  [1 (match f
                       ["-" (cons 'None (puzzle-cells (drop 1 cells)))]
                       [_ (cons (Some (LetterCell (string-ref f 0)))
                                (puzzle-cells (drop 1 cells)))])]
                  [_
                   (cons
                    (Some
                     (NumberedCell
                      (string-ref (list-ref (split-string f #\.) 1) 0)
                      (cast
                       (string->number (list-ref (split-string f #\.) 0))
                       Integer)))
                    (puzzle-cells (drop 1 cells)))])]))
(check-expect (puzzle-cells (get-puzzle-cells (get-puzzle-cells-lines
                                               "nyt-mini-nov-27-2020.xwp")))
              (list 'None (Some (NumberedCell #\F 1))
                    (Some (NumberedCell #\O 2)) (Some (NumberedCell #\I 3))
                    (Some (NumberedCell #\L 4)) 'None
                    (Some (NumberedCell #\L 5)) (Some (LetterCell #\U))
                    (Some (LetterCell #\K)) (Some (LetterCell #\E))
                    (Some (NumberedCell #\J 6)) (Some (LetterCell #\A))
                    (Some (LetterCell #\N)) (Some (LetterCell #\E))
                    (Some (LetterCell #\T)) (Some (NumberedCell #\O 7))
                    (Some (LetterCell #\R)) (Some (LetterCell #\C))
                    (Some (LetterCell #\A)) 'None (Some (NumberedCell #\B 8))
                    (Some (LetterCell #\E)) (Some (LetterCell #\E))
                    (Some (LetterCell #\S)) 'None))

(: get-puzzle-clues-lines : String -> (Listof String))
;; return a list of strings that contains only the lines formatted as crossword
;; puzzle clues from a xwp file
(define (get-puzzle-clues-lines file)
  (local
    {(define puzzle-list (file->lines file))}
    (drop (get-index-clues puzzle-list 0) puzzle-list)))
(check-expect
 (get-puzzle-clues-lines "nyt-mini-nov-27-2020.xwp")
 '("1across:Leftovers' cover"
   "5across:Eldest of Hollywood's Hemsworth brothers"
   "6across:Yellen of the Biden cabinet"
   "7across:Killer whale"
   "8across:Fuzzy buzzers"
   "1down:Roadside warning"
   "2down:1/8 cup"
   "3down:Furniture stores that average 300,000 square feet \
(or five football fields!)"
   "4down:Tennis do-over"
   "6down:\"You had one ___!\""))

(: get-letter-index : String Integer -> Integer)
;; return the index of the first letter in a given string
;; assuming format is correctly given as a clue
;; when called... counter must be 0
(define (get-letter-index line counter)
  (match (string->number (substring line counter (+ 1 counter)))
    [#f counter]
    [_ (get-letter-index line (+ 1 counter))]))
(check-expect (get-letter-index "1test" 0) 1)
(check-expect (get-letter-index "500test" 0) 3)

(: puzzle-clues : (Listof String) -> (Listof Clue))
;; get the clues from a xwp file
;; given a list of strings rather than the file name
(define (puzzle-clues clues)
  (match clues
    ['() '()]
    [(cons f r) (local
                  {(define letter-index (get-letter-index f 0))
                   (define number
                     (cast
                      (string->number (substring f 0 letter-index))
                      Integer))
                   (define direction
                     (substring
                      (list->string
                       (first-split (string->list f) #\:)) letter-index))
                   (define text
                     (list->string (rest-split (string->list f) #\:)))}
                  (match direction
                    ["across"
                     (cons (Clue number 'across text) (puzzle-clues r))]
                    ["down"
                     (cons (Clue number 'down text) (puzzle-clues r))]))]))
(check-expect
 (puzzle-clues (get-puzzle-clues-lines "nyt-mini-nov-27-2020.xwp"))
 (list
  (Clue 1 'across "Leftovers' cover")
  (Clue 5 'across "Eldest of Hollywood's Hemsworth brothers")
  (Clue 6 'across "Yellen of the Biden cabinet")
  (Clue 7 'across "Killer whale")
  (Clue 8 'across "Fuzzy buzzers")
  (Clue 1 'down "Roadside warning")
  (Clue 2 'down "1/8 cup")
  (Clue 3 'down "Furniture stores that average 300,000 square feet \
(or five football fields!)")
  (Clue 4 'down "Tennis do-over")
  (Clue 6 'down "\"You had one ___!\"")))

(: puzzle-from-file : String -> CrosswordPuzzle)
;; load CrosswordPuzzle from xwp file
(define (puzzle-from-file file)
  (CrosswordPuzzle
   (puzzle-col file)
   (puzzle-row file)
   (puzzle-cells (get-puzzle-cells (get-puzzle-cells-lines file)))
   (puzzle-clues (get-puzzle-clues-lines file))
   (puzzle-source file)))
(check-expect
 (puzzle-from-file "nyt-mini-nov-27-2020.xwp")
 (CrosswordPuzzle
  5
  5
  (list 'None (Some (NumberedCell #\F 1)) (Some (NumberedCell #\O 2))
        (Some (NumberedCell #\I 3)) (Some (NumberedCell #\L 4)) 'None
        (Some (NumberedCell #\L 5)) (Some (LetterCell #\U))
        (Some (LetterCell #\K)) (Some (LetterCell #\E))
        (Some (NumberedCell #\J 6)) (Some (LetterCell #\A))
        (Some (LetterCell #\N)) (Some (LetterCell #\E)) (Some (LetterCell #\T))
        (Some (NumberedCell #\O 7)) (Some (LetterCell #\R))
        (Some (LetterCell #\C)) (Some (LetterCell #\A)) 'None
        (Some (NumberedCell #\B 8)) (Some (LetterCell #\E))
        (Some (LetterCell #\E)) (Some (LetterCell #\S)) 'None)
  (list (Clue 1 'across "Leftovers' cover")
        (Clue 5 'across "Eldest of Hollywood's Hemsworth brothers")
        (Clue 6 'across "Yellen of the Biden cabinet")
        (Clue 7 'across "Killer whale")
        (Clue 8 'across "Fuzzy buzzers")
        (Clue 1 'down "Roadside warning")
        (Clue 2 'down "1/8 cup")
        (Clue 3 'down "Furniture stores that average 300,000 square feet \
(or five football fields!)")
        (Clue 4 'down "Tennis do-over")
        (Clue 6 'down "\"You had one ___!\""))
  (Some "New York Times Mini, Nov 27, 2020")))
(check-expect
 (puzzle-from-file "nyt-jan-25-2016.xwp")
 (CrosswordPuzzle
  15
  15
  (list (Some (NumberedCell #\L 1)) (Some (NumberedCell #\E 2))
        (Some (NumberedCell #\F 3)) (Some (NumberedCell #\T 4)) 'None
        (Some (NumberedCell #\A 5)) (Some (NumberedCell #\L 6))
        (Some (NumberedCell #\S 7)) (Some (NumberedCell #\O 8)) 'None 'None
        (Some (NumberedCell #\A 9)) (Some (NumberedCell #\B 10))
        (Some (NumberedCell #\C 11)) (Some (NumberedCell #\S 12))
        (Some (NumberedCell #\I 13)) (Some (LetterCell #\R))
        (Some (LetterCell #\I)) (Some (LetterCell #\S)) 'None
        (Some (NumberedCell #\N 14)) (Some (LetterCell #\O))
        (Some (LetterCell #\K)) (Some (LetterCell #\I))
        (Some (NumberedCell #\A 15)) 'None (Some (NumberedCell #\T 16))
        (Some (LetterCell #\O)) (Some (LetterCell #\O)) (Some (LetterCell #\L))
        (Some (NumberedCell #\M 17)) (Some (LetterCell #\A))
        (Some (LetterCell #\S)) (Some (LetterCell #\K))
        (Some (NumberedCell #\E 18)) (Some (LetterCell #\D))
        (Some (LetterCell #\B)) (Some (LetterCell #\A)) (Some (LetterCell #\L))
        (Some (LetterCell #\L)) 'None (Some (NumberedCell #\T 19))
        (Some (LetterCell #\O)) (Some (LetterCell #\N)) (Some (LetterCell #\E))
        (Some (NumberedCell #\A 20)) (Some (LetterCell #\S))
        (Some (LetterCell #\H)) 'None (Some (NumberedCell #\A 21))
        (Some (LetterCell #\R)) (Some (LetterCell #\E)) 'None
        (Some (NumberedCell #\S 22)) (Some (LetterCell #\T))
        (Some (NumberedCell #\A 23)) (Some (LetterCell #\I))
        (Some (LetterCell #\N)) (Some (LetterCell #\E)) (Some (LetterCell #\D))
        'None 'None (Some (NumberedCell #\S 24)) (Some (NumberedCell #\T 25))
        (Some (LetterCell #\R)) (Some (LetterCell #\E)) (Some (LetterCell #\S))
        (Some (NumberedCell #\S 26)) 'None (Some (NumberedCell #\A 27))
        (Some (LetterCell #\L)) (Some (LetterCell #\L)) (Some (LetterCell #\E))
        (Some (LetterCell #\Y)) (Some (LetterCell #\S))
        (Some (NumberedCell #\M 28)) (Some (NumberedCell #\A 29))
        (Some (LetterCell #\T)) (Some (LetterCell #\E)) (Some (LetterCell #\S))
        'None 'None (Some (NumberedCell #\K 30)) (Some (NumberedCell #\A 31))
        (Some (LetterCell #\R)) (Some (LetterCell #\M)) (Some (LetterCell #\A))
        'None 'None 'None (Some (NumberedCell #\A 32)) (Some (LetterCell #\V))
        (Some (LetterCell #\I)) (Some (LetterCell #\D)) 'None
        (Some (NumberedCell #\T 33)) (Some (NumberedCell #\H 34))
        (Some (LetterCell #\E)) (Some (LetterCell #\N)) (Some (LetterCell #\B))
        (Some (LetterCell #\A)) 'None (Some (NumberedCell #\F 35))
        (Some (NumberedCell #\O 36)) (Some (NumberedCell #\E 37))
        (Some (NumberedCell #\M 38)) (Some (LetterCell #\I))
        (Some (LetterCell #\C)) 'None (Some (NumberedCell #\C 39))
        (Some (LetterCell #\H)) (Some (LetterCell #\E)) (Some (LetterCell #\W))
        (Some (LetterCell #\T)) (Some (LetterCell #\O)) (Some (LetterCell #\Y))
        'None (Some (NumberedCell #\U 40)) (Some (LetterCell #\V))
        (Some (LetterCell #\A)) (Some (NumberedCell #\A 41))
        (Some (LetterCell #\S)) (Some (LetterCell #\K)) 'None
        (Some (NumberedCell #\H 42)) (Some (LetterCell #\E))
        (Some (LetterCell #\R)) (Some (LetterCell #\E)) (Some (LetterCell #\S))
        (Some (LetterCell #\Y)) 'None (Some (NumberedCell #\O 43))
        (Some (LetterCell #\N)) (Some (LetterCell #\E)) (Some (LetterCell #\S))
        'None 'None 'None (Some (NumberedCell #\D 44)) (Some (LetterCell #\I))
        (Some (LetterCell #\N)) (Some (LetterCell #\E)) (Some (LetterCell #\R))
        'None 'None (Some (NumberedCell #\N 45)) (Some (LetterCell #\A))
        (Some (LetterCell #\N)) (Some (LetterCell #\N)) (Some (LetterCell #\Y))
        (Some (NumberedCell #\S 46)) (Some (NumberedCell #\H 47))
        (Some (NumberedCell #\R 48)) (Some (LetterCell #\I))
        (Some (LetterCell #\N)) (Some (LetterCell #\E)) 'None
        (Some (NumberedCell #\S 49)) (Some (NumberedCell #\A 50))
        (Some (NumberedCell #\F 51)) (Some (LetterCell #\E))
        (Some (LetterCell #\T)) (Some (LetterCell #\Y)) 'None 'None
        (Some (NumberedCell #\L 52)) (Some (LetterCell #\E))
        (Some (LetterCell #\O)) (Some (LetterCell #\N)) (Some (LetterCell #\A))
        (Some (LetterCell #\R)) (Some (NumberedCell #\D 53)) 'None
        (Some (NumberedCell #\N 54)) (Some (LetterCell #\E))
        (Some (LetterCell #\W)) 'None (Some (NumberedCell #\B 55))
        (Some (NumberedCell #\O 56)) (Some (NumberedCell #\B 57))
        (Some (NumberedCell #\E 58)) (Some (LetterCell #\A))
        (Some (LetterCell #\V)) (Some (LetterCell #\E)) 'None
        (Some (NumberedCell #\V 59)) (Some (LetterCell #\E))
        (Some (NumberedCell #\L 60)) (Some (LetterCell #\V))
        (Some (LetterCell #\E)) (Some (LetterCell #\T))
        (Some (NumberedCell #\R 61)) (Some (LetterCell #\O))
        (Some (LetterCell #\P)) (Some (LetterCell #\E))
        (Some (NumberedCell #\E 62)) (Some (LetterCell #\V))
        (Some (LetterCell #\E)) (Some (LetterCell #\R)) 'None
        (Some (NumberedCell #\E 63)) (Some (LetterCell #\M))
        (Some (LetterCell #\A)) (Some (LetterCell #\I)) (Some (LetterCell #\L))
        'None (Some (NumberedCell #\O 64)) (Some (LetterCell #\N))
        (Some (LetterCell #\U)) (Some (LetterCell #\S))
        (Some (NumberedCell #\P 65)) (Some (LetterCell #\E))
        (Some (LetterCell #\S)) (Some (LetterCell #\O)) 'None 'None
        (Some (NumberedCell #\O 66)) (Some (LetterCell #\W))
        (Some (LetterCell #\L)) (Some (LetterCell #\S)) 'None
        (Some (NumberedCell #\W 67)) (Some (LetterCell #\E))
        (Some (LetterCell #\S)) (Some (LetterCell #\T)))
  (list (Clue 1 'across "Liberals, with \"the\"")
        (Clue 5 'across "Too")
        (Clue 9 'across "Basics of education")
        (Clue 13 'across "A pupil is in the middle of it")
        (Clue 14 'across "Finnish-based telecom")
        (Clue 16 'across "Screwdriver or hammer")
        (Clue 17 'across "*Party with disguises")
        (Clue 19 'across "Build one's muscles, with \"up\"")
        (Clue 20 'across "Something an e-cig lacks")
        (Clue 21 'across "\"...and yet, here we ___\"")
        (Clue 22 'across "Like the glass in some church windows")
        (Clue 24 'across "Emphasis")
        (Clue 27 'across "Bowling lanes")
        (Clue 28 'across "Endings of chess games")
        (Clue 30 'across "Cosmic destiny")
        (Clue 32 'across "Like devoted fans")
        (Clue 33 'across "What Grizzlies and Timberwolves play in")
        (Clue 35 'across "Enemy")
        (Clue 38 'across "Plug-in in an amp")
        (Clue 39 'across "Puppy amuser ... \
or the end of the answer to each starred clue")
        (Clue 40 'across "Sch. founded by Thomas Jefferson")
        (Clue 41 'across "Act as a quizmaster")
        (Clue 42 'across "Crime that Joan of Arc was charged with")
        (Clue 43 'across "Change for a five")
        (Clue 44 'across "Casual eatery")
        (Clue 45 'across "Job for Mrs. Doubtfire or Mary Poppins")
        (Clue 46 'across "Pilgrimage site")
        (Clue 49 'across "It's worth two points in football")
        (Clue 52 'across "Conductor Bernstein")
        (Clue 54 'across "Hot off the presses")
        (Clue 55 'across "Move like a buoy")
        (Clue 58 'across "Icicle site")
        (Clue 59 'across "*Barrier outside a popular nightclub")
        (Clue 62 'across "\"For the first time ____...\"")
        (Clue 63 'across "AOL service")
        (Clue 64 'across "Burden")
        (Clue 65 'across "Money in Mexico")
        (Clue 66 'across "Birds whose heads can move 270 degrees")
        (Clue 67 'across "Toward sunset")
        (Clue 1 'down "World capital whose name is a kind of bean")
        (Clue 2 'down "Noteworthy periods")
        (Clue 3 'down "*Food item often dipped in ketchup or tartar sauce")
        (Clue 4 'down "\"'Tis a pity\"")
        (Clue 5 'down "Actor Braugher of \"Brooklyn Nine-Nine\"")
        (Clue 6 'down "Lower parts of 18-Down")
        (Clue 7 'down "Jamaican music genre")
        (Clue 8 'down "Lubricates")
        (Clue 9 'down "Notable Hun")
        (Clue 10 'down "Frontiersman Daniel")
        (Clue 11 'down "New York's _____ Island")
        (Clue 12 'down "Winter coasters")
        (Clue 15 'down "Mass assistant")
        (Clue 18 'down "Headphones cover them")
        (Clue 23 'down "Cosmetics brand owned by Revlon")
        (Clue 25 'down "Kennedy who said \"Frankly, \
I don't mind not being president\"")
        (Clue 26 'down "Shish kebab holders")
        (Clue 28 'down "Call from a crib")
        (Clue 29 'down "Hertz rival")
        (Clue 31 'down "Insects in colonies")
        (Clue 33 'down "\"Where does that guy get off?!\"")
        (Clue 34 'down "\"____ comes trouble!\"")
        (Clue 35 'down "*Inaptly named part of the elbow")
        (Clue 36 'down "Where a cake is baked")
        (Clue 37 'down "\"Piece of cake\"")
        (Clue 39 'down "Shanghai's land")
        (Clue 43 'down "Cereal grain")
        (Clue 44 'down "Money, in Mexico")
        (Clue 45 'down "Politico Gingrich")
        (Clue 46 'down "Snooze")
        (Clue 47 'down "Throw, as an anchor")
        (Clue 48 'down "Wanders around")
        (Clue 50 'down "Blacksmith's block")
        (Clue 51 'down "Senses")
        (Clue 53 'down "Floor model")
        (Clue 56 'down "___ Dei (Catholic group)")
        (Clue 57 'down "A+++")
        (Clue 60 'down "Bill, after being signed by the president")
        (Clue 61 'down "Column's counterpart"))
  (Some "New York Times, Monday Jan 25, 2016 by Ian Livengood")))

(: draw : CrossWorld -> Image)
;; draw the CrossWorld appropriately with respect to
;; size, cell type, clues, source, solution, click distinction, etc...
(define (draw world)
  (match world
    [(CrossWorld size puzzle click dir index cand display check)
     (match puzzle
       [(CrosswordPuzzle col _ cells clues source)
        (local
          {(define image (above/align
                          "left"
                          (beside/align
                           "top"
                           (draw-puzzle puzzle size display check cand)
                           (rectangle 30 1 'solid 'white)
                           (draw-clues clues
                                       (find-clue cells col index dir)
                                       dir))
                          (local
                            {(define check?
                               (if check
                                   (text "checking: on" 12 'black)
                                   (text "checking: off" 12 'black)))}
                            (match source
                              ['None check?]
                              [(Some value)
                               (above/align
                                "left"
                                (rectangle 1 30 'solid 'white)
                                (beside
                                 check?
                                 (rectangle 30 1 'solid 'white)
                                 (text value 12 'black)))]))))}
          (match click
            ['None image]
            [(Some (Click x y))
             (match dir
               ['None empty-image]
               [(Some direction)
                (match direction
                  ['across (underlay/align/offset
                            "left" "top"
                            (draw-arrow-across size)
                            (- x) (+ (- y) (- (/ size 2)) (* (/ size 28) 15/2))
                            image)]
                  ['down (underlay/align/offset
                          "left" "top"
                          (draw-arrow-down size)
                          (+ (- x) (- (/ size 2)) (* (/ size 28) 15/2)) (- y)
                          image)])])]))])]))
(draw
 (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None
             (make-list (length (CrosswordPuzzle-cells nyt-mini-nov-10-2020))
                        'None)
             #t
             #f))
(draw
 (CrossWorld 60 mini-rectangle (Some (Click 120 60)) (Some 'down) (Some 7)
             (list (Some #\A) 'None 'None 'None (Some #\B)
                   'None 'None (Some #\C) 'None 'None
                   (Some #\D) 'None 'None 'None (Some #\E)) 
             #f
             #t))
(draw
 (CrossWorld 40 minimal-puzzle 'None 'None 'None
             (make-list (length (CrosswordPuzzle-cells minimal-puzzle))
                        'None)
             #f
             #f))
         
(: react-to-mouse : CrossWorld Integer Integer Mouse-Event -> CrossWorld)
;; if the user clicks on either a LetterCell or NumberedCell (aka white square),
;; distinguish the cell with either an across or down arrow and
;; distinguish the associated clue (if applicable)
;; if the user clicks outside such a cell, no distinction
(define (react-to-mouse world x y e)
  (match e
    ["button-down"
     (match world
       [(CrossWorld size puzzle last-click dir index cand display check)
        (match (in-a-square?
                size
                (Click x y)
                (top-left world 1 0 0)
                0)
          ['Not
           (CrossWorld size puzzle 'None dir 'None cand display check)]         
          [(Click xcorner ycorner)
           (CrossWorld
            size
            puzzle
            (Some (Click xcorner ycorner))
            dir
            (square-index size (Click x y) (top-left world 1 0 0) 0)
            cand
            display
            check)])])]             
    [_ world]))
(check-expect
 (react-to-mouse
  (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #t)
  45
  45
  "button-down")
 (CrossWorld
  30 nyt-mini-nov-10-2020 (Some (Click 30 30)) 'None (Some 6) '() #f #t))
(check-expect
 (react-to-mouse
  (CrossWorld
   30 nyt-mini-nov-10-2020 (Some (Click 30 30)) 'None (Some 6) '() #t #f)
  45
  45
  "move")
 (CrossWorld
  30 nyt-mini-nov-10-2020 (Some (Click 30 30)) 'None (Some 6) '() #t #f))
(check-expect
 (react-to-mouse
  (CrossWorld
   30 nyt-mini-nov-10-2020 (Some (Click 30 30)) 'None (Some 6) '() #f #f)
  90
  151
  "button-down")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f))

(: react-to-key : CrossWorld String -> CrossWorld)
;; if the user clicks the "escape" key, toggle the solution display
;; if the user clicks the "tab" key, toggle across and down mode
;; if the user clicks the right arrow key, set to across mode
;; if the user clicks the down arrow key, set to down mode
;; if the user clicks the "`" key, reset the puzzle
;; if the user clicks the "return" key, toggle the checking mode
;; if the user clicks any letter, enter the corresponding uppercase letter
;; into the puzzle at the correct cell (if applicable)
;; anything else has no effect
(define (react-to-key world k)
  (match world
    [(CrossWorld size puzzle last-click dir index cand display check)
     (match k
       ["escape"
        (if display
            (CrossWorld size puzzle last-click dir index cand #f check)
            (CrossWorld size puzzle last-click dir index cand #t check))]
       ["\t" (match dir
               [(Some 'across)
                (CrossWorld size puzzle last-click
                            (Some 'down) index cand display check)]
               [(Some 'down)
                (CrossWorld size puzzle last-click
                            (Some 'across) index cand display check)])]
       ["right" (CrossWorld size puzzle last-click
                            (Some 'across) index cand display check)]
       ["down" (CrossWorld size puzzle last-click
                           (Some 'down) index cand display check)]
       ["`" (CrossWorld size puzzle 'None
                        dir 'None
                        (make-list (length (CrosswordPuzzle-cells puzzle))
                                   'None)
                        #f
                        check)]
       ["\r" (match check
               [#f
                (CrossWorld size puzzle last-click dir index cand display #t)]
               [#t
                (CrossWorld size puzzle last-click dir index cand display #f)])]
       [letter? (if (and
                     (= (string-length letter?) 1)
                     (char-alphabetic? (string-ref letter? 0)))
                    (CrossWorld
                     size
                     puzzle
                     last-click
                     dir
                     index
                     (match index
                       ['None cand]
                       [(Some index)
                        (replace-at
                         index
                         (char-upcase (string-ref letter? 0))
                         cand)])
                     display
                     check)
                    world)])]))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #f #f)
  "escape")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #t #f))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'across) 'None '() #t #f)
  "\t")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'down) 'None '() #t #f))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'down) 'None '() #f #f)
  "right")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'across) 'None '() #f #f))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'across) 'None '() #f #f)
  "down")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'down) 'None '() #f #f))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 (Some (Click 30 30)) (Some 'down) (Some 6)
              (list 'None (Some #\A) 'None (Some #\A) 'None
                    (Some #\A) 'None (Some #\A) 'None (Some #\A)
                    'None (Some #\A) 'None (Some #\A) 'None
                    (Some #\A) 'None (Some #\A) 'None (Some #\A))
              #f
              #t)                
  "`")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'down) 'None
             (make-list 25 'None) #f #t))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'across) 'None '() #f #t)
  "\r")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None (Some 'across) 'None '() #f #f))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 (Some (Click 30 30)) (Some 'down) (Some 6)
              (list 'None (Some #\A) 'None (Some #\A) 'None
                    (Some #\A) 'None (Some #\A) 'None (Some #\A)
                    'None (Some #\A) 'None (Some #\A) 'None
                    (Some #\A) 'None (Some #\A) 'None (Some #\A))
              #f
              #t)                
  "z")
 (CrossWorld 30 nyt-mini-nov-10-2020 (Some (Click 30 30)) (Some 'down) (Some 6)
             (list 'None (Some #\A) 'None (Some #\A) 'None
                   (Some #\A) (Some #\Z) (Some #\A) 'None (Some #\A)
                   'None (Some #\A) 'None (Some #\A) 'None
                   (Some #\A) 'None (Some #\A) 'None (Some #\A))
             #f #t))
(check-expect
 (react-to-key
  (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #t #f)
  "@")
 (CrossWorld 30 nyt-mini-nov-10-2020 'None 'None 'None '() #t #f))

(: run : Integer CrosswordPuzzle -> CrossWorld)
;; the arguments to run are the side length in pixels of each puzzle cell
;; and the puzzle to be displayed
(define (run size puzzle)
  (big-bang (CrossWorld size puzzle 'None (Some 'across) 'None
                        (make-list (length (CrosswordPuzzle-cells puzzle))
                                   'None)
                        #f #f) : CrossWorld
    [to-draw draw]
    [on-mouse react-to-mouse]
    [on-key react-to-key]))


(test)