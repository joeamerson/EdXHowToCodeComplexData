;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname accumulator-example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof String) -> (listof String)
;; append each string's position in the list to the front of the string to number the list
(check-expect (number-list empty) empty)
(check-expect (number-list (list "first" "second" "third")) 
              (list "1: first" "2: second" "3: third"))
;;(define (number-list los) los)   ;stub
#; ;;template
(define (number-list los)
  ;; acc: Number; position of (first los) in los0
  (local [(define (number-list los0 acc)
            (cond [(empty? los) empty]
                  [else
                   (... acc
                        (first los)
                        (number-list (rest los) (... acc)))]))]
    (number-list los0 ...)))


(define (number-list los0)
  ;; acc: Number; position of (first los) in los0
  (local [(define (number-list los acc)
            (cond [(empty? los) empty]
                  [else
                   (cons (string-append (number->string acc) ": " (first los))
                         (number-list (rest los) (add1 acc)))]))]
    (number-list los0 1)))