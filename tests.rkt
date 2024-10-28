#lang racket
(require rackunit)
(require "hw74.rkt")  ;; Import the main file

;; Tests for create-TextBox
(check-equal? (TextBox-pre (create-TextBox "ab" "cd")) (list "a" "b"))
(check-equal? (TextBox-post (create-TextBox "ab" "cd")) (list "c" "d"))

;; Tests for TextBox-pre-str
(check-equal? (TextBox-pre-str (create-TextBox "ab" "cd")) "ab")

;; Tests for TextBox-post-str
(check-equal? (TextBox-post-str (create-TextBox "ab" "cd")) "cd")

;; Tests for textbox-delete
(check-equal? (TextBox-post-str (textbox-delete (create-TextBox "ab" "cd"))) "d")

;; Tests for textbox-backspace
(check-equal? (TextBox-pre-str (textbox-backspace (create-TextBox "ab" "cd"))) "a")

;; Tests for textbox-insert
(check-equal? (TextBox-pre-str (textbox-insert (create-TextBox "ab" "cd") "x")) "abx")

;; Tests for textbox-left
(check-equal? 
 (TextBox-pre-str (textbox-left (create-TextBox "ab" "cd"))) 
 "a")

;; Tests for textbox-right
(check-equal? 
 (TextBox-post-str (textbox-right (create-TextBox "ab" "cd"))) 
 "d")
