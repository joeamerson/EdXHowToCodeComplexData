;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname foldr-max-value-example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; (listof Natural) -> Natural
;; Returns the largest value from a list of numbers

(check-expect (pick-max (list 4 8 1 4 2 10)) 10)
(check-expect (pick-max (list 4 6 1)) 6)
(check-expect (pick-max (list -10 -2 1)) 1)

(define (pick-max lon)
  (foldr (λ (n1 n2)
           (if (> n1 n2)
               n1
               n2))
         (first lon)
         (rest lon)))


;; (listof Natural) -> Natural
;; Returns the smallest value from a list of numbers

(check-expect (pick-min (list 4 8 1 4 2 10)) 1)
(check-expect (pick-min (list 4 6 1)) 1)
(check-expect (pick-min (list -10 -2 1)) -10)

;;(define (pick-min lon) 0);stub
(define (pick-min lon)
  (foldr (λ (n1 n2)
           (if (< n1 n2)
               n1
               n2))
         (first lon)
         (rest lon)))

;; (listof Natural) -> Natural | False
;; Returns the largest prime number from a list of numbers (not all prime), or false if no primes in list
;; Assume whole list is positive values, no negatives
(check-expect (pick-largest-prime (list 4 8 1 4 2 10)) 2)
(check-expect (pick-largest-prime (list 4 8 1 4 10)) false)
(check-expect (pick-largest-prime (list 2 3 5 7 11 13 17 19 23 24)) 23)
(check-expect (pick-largest-prime (list 4 8 7 4 2 10)) 7)

;;(define (pick-largest-prime lon) 1);stub
(define (pick-largest-prime lon)
  (foldr (λ (n1 n2)
           (cond [(and (prime? n1) (prime? n2))
                  (if (> n1 n2)
                      n1
                      n2)]
                 [(prime? n1)
                  n1]
                 [(prime? n2)
                  n2]
                 [else
                  false]))   ;; Returns false if no prime numbers in list! 
         (first lon)
         (rest lon)))


;; Natural -> Boolean
;; Returns true if the number is prime
;;(define (prime? n) false);stub

(check-expect (prime? 7) true)
(check-expect (prime? 23) true)
(check-expect (prime? 24) false)
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)
(check-expect (prime? 3) true)

(define (prime? n)
  (cond [(false? n)
         false]
         [(< n 2)
         false]
        [else
         (andmap (λ (num)
                   (not (= (remainder n num) 0))) ;checks to see if not divisible by numbers in list
                 (rest (build-list (sub1 n)
                                   add1)))])) ;;builds list of numbers to test for divisibility, except the first (1)


