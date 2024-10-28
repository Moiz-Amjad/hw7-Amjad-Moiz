#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(provide (all-defined-out))

;; Constants
(define FONT-SIZE 32)
(define FONT-COLOR "black")
(define CURSOR-WIDTH 2)
(define TEXTBOX-WIDTH 512)
(define TEXTBOX-HEIGHT 40)
(define INITIAL-SPACE 4)
(define CURSOR-BLINK-INTERVAL 0.5)

;; Data Definitions
(struct TextBox [pre post cursor-visible?])

;; remove-char : String Natural -> String
;; Removes character at position i from str
(define (remove-char str i)
  (string-append (substring str 0 i)
                (substring str (add1 i))))

;; shift-char : String Natural -> String
;; Shifts character at position i to position i+1
(define (shift-char str i)
  (if (< i (sub1 (string-length str)))
      (string-append
       (substring str 0 i)
       (substring str (add1 i) (add1 (add1 i)))
       (substring str i (add1 i))
       (substring str (add1 (add1 i))))
      str))

;; create-TextBox : String String -> TextBox
(define (create-TextBox pre-str post-str)
  (TextBox pre-str post-str #true))

;; textbox-insert : TextBox String -> TextBox
(define (textbox-insert tb char)
  (TextBox 
   (string-append (TextBox-pre tb) char)
   (TextBox-post tb)
   (TextBox-cursor-visible? tb)))

;; textbox-left : TextBox -> TextBox
(define (textbox-left tb)
  (if (string=? (TextBox-pre tb) "")
      tb
      (TextBox 
       (substring (TextBox-pre tb) 0 (sub1 (string-length (TextBox-pre tb))))
       (string-append 
        (substring (TextBox-pre tb) (sub1 (string-length (TextBox-pre tb))))
        (TextBox-post tb))
       (TextBox-cursor-visible? tb))))

;; textbox-right : TextBox -> TextBox
(define (textbox-right tb)
  (if (string=? (TextBox-post tb) "")
      tb
      (TextBox 
       (string-append (TextBox-pre tb) (substring (TextBox-post tb) 0 1))
       (substring (TextBox-post tb) 1)
       (TextBox-cursor-visible? tb))))

;; textbox-delete : TextBox -> TextBox
(define (textbox-delete tb)
  (TextBox (TextBox-pre tb)
           (if (string=? (TextBox-post tb) "") 
               ""
               (substring (TextBox-post tb) 1))
           (TextBox-cursor-visible? tb)))

;; textbox-backspace : TextBox -> TextBox
(define (textbox-backspace tb)
  (TextBox 
   (if (string=? (TextBox-pre tb) "")
       ""
       (substring (TextBox-pre tb) 0 (sub1 (string-length (TextBox-pre tb)))))
   (TextBox-post tb)
   (TextBox-cursor-visible? tb)))

;; render : TextBox -> Image
(define (render tb)
  (let* ([pre-text (text (TextBox-pre tb) FONT-SIZE FONT-COLOR)]
         [post-text (text (TextBox-post tb) FONT-SIZE FONT-COLOR)]
         [cursor (if (TextBox-cursor-visible? tb)
                    (rectangle CURSOR-WIDTH FONT-SIZE "solid" "black")
                    (rectangle CURSOR-WIDTH FONT-SIZE "solid" "white"))]
         [background (rectangle TEXTBOX-WIDTH TEXTBOX-HEIGHT "outline" "black")]
         [content (beside/align "baseline" pre-text cursor post-text)])
    (overlay/align "left" "center" 
                  (place-image content 
                              (+ INITIAL-SPACE (/ (image-width pre-text) 2))
                              (/ TEXTBOX-HEIGHT 2)
                              background)
                  background)))

;; toggle-cursor : TextBox -> TextBox
(define (toggle-cursor tb)
  (TextBox (TextBox-pre tb)
           (TextBox-post tb)
           (not (TextBox-cursor-visible? tb))))

;; key-handler : TextBox KeyEvent -> TextBox
(define (key-handler tb key)
  (cond
    [(string=? key "left") (textbox-left tb)]
    [(string=? key "right") (textbox-right tb)]
    [(or (string=? key "backspace") (string=? key "\b")) 
     (textbox-backspace tb)]
    [(string=? key "delete") (textbox-delete tb)]
    [(or (string=? key "\r") (string=? key "\n") 
         (string=? key "return") (string=? key "enter")
         (string=? key "\t") (string=? key "tab")) 
     tb]
    [(= (string-length key) 1) (textbox-insert tb key)]
    [else tb]))

;; main : -> TextBox
(define (main)
  (big-bang (create-TextBox "" "")
            [to-draw render]
            [on-key key-handler]
            [on-tick toggle-cursor CURSOR-BLINK-INTERVAL]))

(main)