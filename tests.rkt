#lang racket

(require rackunit
         "hw7.rkt")
(provide TESTS)

;; Test suite for hw7.rkt functions
(define TESTS
  (test-suite
   "hw7.rkt Tests"
   (begin
     ;; Testing create-TextBox
     (test-case "create-TextBox"
       (check-equal? (create-TextBox "hi" "world") (TextBox "hi" "world"))
       (check-equal? (create-TextBox "" "") (TextBox "" ""))
       (check-equal? (create-TextBox "text" "other") (TextBox "text" "other")))

     ;; Testing remove-char
     (test-case "remove-char"
       (check-equal? (remove-char "hello") "ello")
       (check-equal? (remove-char "a") "")
       (check-equal? (remove-char "") ""))

     ;; Testing remove-last-char
     (test-case "remove-last-char"
       (check-equal? (remove-last-char "hello") "hell")
       (check-equal? (remove-last-char "a") "")
       (check-equal? (remove-last-char "") ""))

     ;; Testing shift-char (left to right)
     (test-case "shift-char-left with TextBox"
       (let* ((tb (create-TextBox "hello" "world"))
              (pre (TextBox-pre tb))
              (post (TextBox-post tb)))
         (check-equal? (shift-char pre post #true) '("hell" "oworld")))
       (check-equal? (shift-char "" "abc" #true) '("" "abc"))
       (check-equal? (shift-char "test" "" #true) '("tes" "t")))

     ;; Testing shift-char (right to left)
     (test-case "shift-char-right with TextBox"
       (let* ((tb (create-TextBox "hello" "world"))
              (pre (TextBox-pre tb))
              (post (TextBox-post tb)))
         (check-equal? (shift-char post pre #false) '("orld" "hellow")))
       (check-equal? (shift-char "" "abc" #false) '("bc" "a"))
       (check-equal? (shift-char "test" "" #false) '("est" "t")))

     ;; Testing textbox-insert
     (test-case "textbox-insert"
       (check-equal? (textbox-insert (create-TextBox "hel" "lo") "!") (create-TextBox "hel!" "lo"))
       (check-equal? (textbox-insert (create-TextBox "" "start") "X") (create-TextBox "X" "start"))
       (check-equal? (textbox-insert (create-TextBox "test" "") "?") (create-TextBox "test?" "")))

     ;; Testing textbox-delete
     (test-case "textbox-delete"
       (check-equal? (textbox-delete (create-TextBox "hel" "lo")) (create-TextBox "hel" "o"))
       (check-equal? (textbox-delete (create-TextBox "abc" "")) (create-TextBox "abc" ""))
       (check-equal? (textbox-delete (create-TextBox "" "start")) (create-TextBox "" "tart")))

     ;; Testing textbox-backspace
     (test-case "textbox-backspace"
       (check-equal? (textbox-backspace (create-TextBox "hel" "lo")) (create-TextBox "he" "lo"))
       (check-equal? (textbox-backspace (create-TextBox "a" "")) (create-TextBox "" ""))
       (check-equal? (textbox-backspace (create-TextBox "" "lo")) (create-TextBox "" "lo")))

     ;; Testing textbox-left
     (test-case "textbox-left"
       (check-equal? (textbox-left (create-TextBox "hel" "lo")) (create-TextBox "he" "llo"))
       (check-equal? (textbox-left (create-TextBox "" "world")) (create-TextBox "" "world"))
       (check-equal? (textbox-left (create-TextBox "abc" "")) (create-TextBox "ab" "c")))

     ;; Testing textbox-right
     (test-case "textbox-right"
       (check-equal? (textbox-right (create-TextBox "hel" "lo")) (create-TextBox "hell" "o"))
       (check-equal? (textbox-right (create-TextBox "" "start")) (create-TextBox "s" "tart"))
       (check-equal? (textbox-right (create-TextBox "test" "")) (create-TextBox "test" "")))

     ;; Testing key-handler for 'left' key
     (test-case "key-handler-left"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "left") (create-TextBox "he" "llo"))
       (check-equal? (key-handler (create-TextBox "" "world") "left") (create-TextBox "" "world"))
       (check-equal? (key-handler (create-TextBox "test" "") "left") (create-TextBox "tes" "t")))

     ;; Testing key-handler for inserting a character
     (test-case "key-handler-insert"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "!") (create-TextBox "hel!" "lo"))
       (check-equal? (key-handler (create-TextBox "" "world") "A") (create-TextBox "A" "world"))
       (check-equal? (key-handler (create-TextBox "test" "") "B") (create-TextBox "testB" "")))

     ;; Testing key-handler for 'backspace' key
     (test-case "key-handler-backspace"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\b") (create-TextBox "he" "lo"))
       (check-equal? (key-handler (create-TextBox "" "lo") "\b") (create-TextBox "" "lo"))
       (check-equal? (key-handler (create-TextBox "a" "") "\b") (create-TextBox "" "")))

     ;; Testing key-handler for 'delete' key
     (test-case "key-handler-delete"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\u007F") (create-TextBox "hel" "o"))
       (check-equal? (key-handler (create-TextBox "" "world") "\u007F") (create-TextBox "" "orld"))
       (check-equal? (key-handler (create-TextBox "text" "") "\u007F") (create-TextBox "text" "")))

     ;; Testing key-handler for 'enter' key (ignored)
     (test-case "key-handler-enter"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\r") (create-TextBox "hel" "lo"))
       (check-equal? (key-handler (create-TextBox "" "world") "\r") (create-TextBox "" "world"))
       (check-equal? (key-handler (create-TextBox "text" "") "\r") (create-TextBox "text" ""))))))

;; Run the tests
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
