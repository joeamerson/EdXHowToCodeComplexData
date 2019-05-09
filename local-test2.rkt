;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local-test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; EVALUATION EXAMPLE

(define b 1)
(+ b
   (local [(define b 2)]
     (* b b))
   b)

(define b 1)
(+ 1
   (local [(define b 2)]
     (* b b))
   b)

;;Renaming
(define b 1)
(+ 1
   (local [(define b_0 2)]
     (* b_0 b_0))
   b)

;;Lifting - lift local definition up to top-most level
;;Replace local with its body
(define b 1)
(define b_0 2)
(+ 1
   (* b_0 b_0)
   b)

(define b 1)
(define b_0 2)
(+ 1
   (* 2 2)
   1)


(define b 1)
(define b_0 2)
(+ 1
   4
   1)


(define b 1)
(define b_0 2)
6
