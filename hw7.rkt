#lang racket

(require 2htdp/image
         2htdp/universe)
(provide (all-defined-out))

;; CONSTANTS
(define FONT-SIZE 32)
(define FONT-COLOR "black")
(define CURSOR-WIDTH 2)
(define TEXTBOX-WIDTH 512)
(define TEXTBOX-HEIGHT 40)
(define INITIAL-SPACE 4)

;; DATA DEFINITIONS AND PREDICATES
;; TextBox is a (create-TextBox [pre : string] [post : string]) where
;; - pre: represents list of strings of length-1 before the cursor
;; - post: represents list of strings of length-1 after the cursor
(struct TextBox (pre post)#:transparent)
(define/contract (create-TextBox pre post)
  (-> string? string? TextBox?)
  (TextBox pre post))
;; EXAMPLES:
;; (create-TextBox "he" "llo")
;; (create-TextBox "Wor" "ld")
;; (create-TextBox "Testi" "ng")


;; HELPER FUNCTIONS
;; remove-last-char : String -> String
;; Removes last character from string
(define/contract (remove-last-char str)
  (-> string? string?)
  (substring str 0 (sub1 (string-length str))))
;; EXAMPLES
;; (remove-last-char "hello") ; => "hell"
;; (remove-last-char "world") ; => "worl"
;; (remove-last-char "Testing") ; => "Testin"

;; remove-char : String -> String
;; Removes first character from string
(define/contract (remove-char str)
  (-> string? string?)
  (substring str 1 (string-length str)))
;; EXAMPLES
;; (remove-char "hello") ; => "ello"
;; (remove-char "world") ; => "orld"
;; (remove-char "Testing") ; => "esting"

;; shift-char : String String Boolean -> (Values String String)
;; Shifts one character between two strings
;; Moves the last character from source to the start of destination if shift-left? is #true
;; Moves the first character from source to the end of destination if shift-left? is #false
(define/contract (shift-char source destination shift-left?)
  (-> string? string? boolean? (values string? string?))
  (cond
    [shift-left?  
     (let* ((last-char (substring source (sub1 (string-length source))))
            (updated-source (substring source 0 (sub1 (string-length source))))
            (updated-dest (string-append last-char destination)))
       (values updated-source updated-dest))]
    [else 
     (let* ((first-char (substring source 0 1))
            (updated-source (substring source 1 (string-length source)))
            (updated-dest (string-append destination first-char)))
       (values updated-source updated-dest))]))
;; EXAMPLES:
;; (shift-char "hello" "world" #false) 
;; (shift-char "world" "testing" #true)


;; FUNCTIONS
;; textbox-insert : TextBox String -> TextBox
;; Inserts a single-character string at the cursor position
(define/contract (textbox-insert textbox char)
  (-> TextBox? string? TextBox?)
  (let* ((pre-text (TextBox-pre textbox))
         (post-text (TextBox-post textbox))
         (updated-pre (string-append pre-text char))) 
    (create-TextBox updated-pre post-text)))
;; EXAMPLES
;; (textbox-insert (create-TextBox "hel" "lo") "!")
;; (textbox-insert (create-TextBox "" "") "A")
;; (textbox-insert (create-TextBox "test" "") "x")

;; textbox-delete : TextBox -> TextBox
;; Deletes the character immediately after the cursor
(define/contract (textbox-delete textbox)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) textbox)
  (if (zero? (string-length post)) 
      textbox
      (create-TextBox pre (substring post 1))))
;; EXAMPLES
;; (textbox-delete (create-TextBox "hel" "lo"))
;; (textbox-delete (create-TextBox "hello" ""))
;; (textbox-delete (create-TextBox "text" "case"))

;; textbox-backspace : TextBox -> TextBox
;; Deletes the character immediately before the cursor
(define/contract (textbox-backspace textbox)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) textbox)
  (if (zero? (string-length pre)) 
      textbox
      (create-TextBox (substring pre 0 (sub1 (string-length pre))) post)))
;; EXAMPLES
;; (textbox-backspace (create-TextBox "hel" "lo"))
;; (textbox-backspace (create-TextBox "" "world"))
;; (textbox-backspace (create-TextBox "test" ""))

;; textbox-left : TextBox -> TextBox
;; Moves the cursor one character to the left
(define/contract (textbox-left textbox)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) textbox)
  (let-values ([(updated-pre updated-post) (shift-char pre post #true)])
    (create-TextBox updated-pre updated-post)))
;; EXAMPLES
;; (textbox-left (create-TextBox "hel" "lo"))
;; (textbox-left (create-TextBox "a" "bc"))
;; (textbox-left (create-TextBox "word" "s"))

;; textbox-right : TextBox -> TextBox
;; Moves the cursor one character to the right
(define/contract (textbox-right textbox)
  (-> TextBox? TextBox?)
  (match-define (TextBox pre post) textbox)
  (let-values ([(updated-pre updated-post) (shift-char post pre #false)])
    (create-TextBox updated-post updated-pre)))
;; EXAMPLES
;; (textbox-right (create-TextBox "hel" "lo"))
;; (textbox-right (create-TextBox "" "text"))
;; (textbox-right (create-TextBox "ab" "cd"))

;; key-handler : TextBox String -> TextBox
;; Processes key events to update the TextBox state
(define/contract (key-handler textbox key)
  (-> TextBox? string? TextBox?)
  (cond
    [(equal? key "left") (textbox-left textbox)]
    [(equal? key "right") (textbox-right textbox)]
    [(equal? key "\b") (textbox-backspace textbox)]  ;; Backspace
    [(equal? key "\u007F") (textbox-delete textbox)] ;; Delete
    [(or (equal? key "\r") (equal? key "\t")) textbox]  ;; Ignore Enter and Tab
    [(and (string? key) (= (string-length key) 1)) 
     (textbox-insert textbox key)] 
    [else textbox])) 
;; EXAMPLES
;; (key-handler (create-TextBox "hel" "lo") "left")
;; (key-handler (create-TextBox "" "hello") "right")
;; (key-handler (create-TextBox "test" "ing") "\b")

;; render : TextBox -> Image
;; Renders the TextBox state with properly positioned text and cursor
(define/contract (render textbox)
  (-> TextBox? image?)
  (match-define (TextBox pre post) textbox)

  (define pre-text-img  (text pre FONT-SIZE FONT-COLOR))
  (define post-text-img (text post FONT-SIZE FONT-COLOR))
  (define cursor-img (rectangle CURSOR-WIDTH FONT-SIZE 'solid FONT-COLOR))
  (define textbox-outline (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT 'outline 'black))

  ;; Calculate positions
  (define pre-width (image-width pre-text-img))
  (define cursor-x (+ INITIAL-SPACE pre-width))
  (define text-y (/ TEXTBOX-HEIGHT 2))
  (define center-pre (/ pre-width 2))
  (define center-post (+ (/ (image-width post-text-img) 2) pre-width))

  ;; Build the scene
  (define base-scene (place-image textbox-outline (/ TEXTBOX-WIDTH 2) (/ TEXTBOX-HEIGHT 2)
                                  (empty-scene TEXTBOX-WIDTH TEXTBOX-HEIGHT)))
  (define scene-with-cursor (place-image cursor-img cursor-x text-y base-scene))
  (define scene-with-pre (place-image pre-text-img (+ INITIAL-SPACE center-pre) text-y scene-with-cursor))
  (place-image post-text-img (+ INITIAL-SPACE center-post) text-y scene-with-pre))
;; EXAMPLES
;; (render (create-TextBox "hello" "world"))
;; (render (create-TextBox "" ""))
;; (render (create-TextBox "testing" ""))

;; main : -> Void
;; Starts the big-bang program with the initial TextBox state
(define/contract (main)
  (-> void?)
  (big-bang (create-TextBox "" "")
            (on-key key-handler)
            (to-draw render)))

;; (main)
