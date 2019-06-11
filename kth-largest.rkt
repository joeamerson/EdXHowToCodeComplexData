;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname kth-largest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)

;; Integer (listof Number) -> Number
;; Given a list of integers (let's say unsorted), what's the kth largest in the list?

;;(define (kth-largest k lon) 0);stub
(check-expect (kth-largest 3 (list 0 3 5 6)) 3)
(check-expect (kth-largest 2 (list 0 3 5 6)) 5)
(check-expect (kth-largest 1 (list 0 3 5 6)) 6)

(define (kth-largest k lon)
  (kth-element k (sort2 lon)))

;; (listof Number) -> (listof Number)
;; Sorts a list from largest to smallest
;;(define (sort-list lon) empty);stub
(check-expect (sort2 (list 4 2 5 6)) (list 6 5 4 2))
(check-expect (sort2 empty) empty)

(define (sort2 alon) 
  (cond 
    [(empty? alon) empty] 
    [else (insert (first alon)               ;; insert the first from alon
                  (sort2 (rest alon)))]))    ;; into a sorted list

(define (insert n alon) 
  (cond 
    [(empty? alon)
     (cons n empty)] 
    [else
     (if (> n (first alon))                  ;; if n is greater than the first in the list
         (cons n alon)                       ;; add n to the front of the list
         (cons (first alon)                  ;; otherwise put hte first from the list 
               (insert n (rest alon))))]))   ;; back onto front & continue to try to 
                                             ;; insert in rest of list
;; Integer (listof Number) -> Number
;; Returns a kth element from a list
;;(define (kth-element k lon) 0);stub

(check-expect (kth-element 1 (list 6 5 4 3 2 1)) 6)
(check-expect (kth-element 2 (list 6 5 4 3 2 1)) 5)

(define (kth-element k lon)
  (list-ref lon (- k 1)))