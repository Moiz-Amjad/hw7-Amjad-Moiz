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
       (check-equal? (create-TextBox "test" "data") (TextBox "test" "data")))

     ;; Testing remove-char 
     (test-case "remove-char"
       (check-equal? (remove-char "hello") "ello")
       (check-equal? (remove-char "a") ""))

     ;; Testing remove-last-char 
     (test-case "remove-last-char"
       (check-equal? (remove-last-char "hello") "hell")
       (check-equal? (remove-last-char "a") ""))

     ;; Testing shift-char (left to right)
     (test-case "shift-char-left with TextBox"
       (let* ((tb (create-TextBox "hello" "world"))
              (pre (TextBox-pre tb))
              (post (TextBox-post tb)))
         (check-equal? (shift-char pre post #t) (TextBox "hell" "oworld")))
       (check-equal? (shift-char "ab" "cd" #true) (list "a" "bcd")))

     ;; Testing shift-char (right to left)
     (test-case "shift-char-right with TextBox"
       (let* ((tb (create-TextBox "hello" "world"))
              (pre (TextBox-pre tb))
              (post (TextBox-post tb)))
         (check-equal? (shift-char post pre #f) (TextBox "orld" "hellow")))
       (check-equal? (shift-char "xyz" "a" #false) (list "yz" "xa")))

     ;; Testing textbox-insert
     (test-case "textbox-insert"
       (check-equal? (textbox-insert (create-TextBox "hel" "lo") "!") (create-TextBox "hel!" "lo"))
       (check-equal? (textbox-insert (create-TextBox "text" "") "?") (create-TextBox "text?" "")))

     ;; Testing textbox-delete
     (test-case "textbox-delete"
       (check-equal? (textbox-delete (create-TextBox "hel" "lo")) (create-TextBox "hel" "o"))
       (check-equal? (textbox-delete (create-TextBox "abc" "def")) (create-TextBox "abc" "ef")))

     ;; Testing textbox-backspace
     (test-case "textbox-backspace"
       (check-equal? (textbox-backspace (create-TextBox "hel" "lo")) (create-TextBox "he" "lo"))
       (check-equal? (textbox-backspace (create-TextBox "ab" "cd")) (create-TextBox "a" "cd")))

     ;; Testing textbox-left 
     (test-case "textbox-left"
       (check-equal? (textbox-left (create-TextBox "hel" "lo")) (create-TextBox "he" "llo"))
       (check-equal? (textbox-left (create-TextBox "abc" "def")) (create-TextBox "ab" "cdef")))

     ;; Testing textbox-right
     (test-case "textbox-right"
       (check-equal? (textbox-right (create-TextBox "hel" "lo")) (create-TextBox "hell" "o"))
       (check-equal? (textbox-right (create-TextBox "abc" "def")) (create-TextBox "abcd" "ef")))

     ;; Testing key-handler for 'left' key 
     (test-case "key-handler-left"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "left") (create-TextBox "he" "llo"))
       (check-equal? (key-handler (create-TextBox "abc" "def") "left") (create-TextBox "ab" "cdef")))

     ;; Testing key-handler for inserting a character
     (test-case "key-handler-insert"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "!") (create-TextBox "hel!" "lo"))
       (check-equal? (key-handler (create-TextBox "text" "") "?") (create-TextBox "text?" "")))

     ;; Testing key-handler for 'backspace' key 
     (test-case "key-handler-backspace"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\b") (create-TextBox "he" "lo"))
       (check-equal? (key-handler (create-TextBox "ab" "cd") "\b") (create-TextBox "a" "cd")))

     ;; Testing key-handler for 'delete' key
     (test-case "key-handler-delete"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\u007F") (create-TextBox "hel" "o"))
       (check-equal? (key-handler (create-TextBox "abc" "def") "\u007F") (create-TextBox "abc" "ef")))

     ;; Testing key-handler for 'enter' key (ignored)
     (test-case "key-handler-enter"
       (check-equal? (key-handler (create-TextBox "hel" "lo") "\r") (create-TextBox "hel" "lo"))
       (check-equal? (key-handler (create-TextBox "text" "") "\r") (create-TextBox "text" ""))))))

;; Run the tests
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
