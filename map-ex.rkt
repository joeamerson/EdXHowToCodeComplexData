;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname map-ex) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; (listof Image) -> (listof Image)
;; Rotates all images in list by 90 degrees

(define GT (triangle 30 "solid" "green"))
(define L1 (list GT GT GT))

;;(define (rotate-all loi) empty);stub
(check-expect (rotate-all empty) empty)
(check-expect (rotate-all L1) (list (rotate 90 GT)
                                    (rotate 90 GT)
                                    (rotate 90 GT)))

(define (rotate-all loi)
  (local [(define (rotate-90 i)
            (rotate 90 i))]
  (map rotate-90 loi)))

;;This ones uses a closure!
(define (rotate-all-by n loi)
  (local [(define (rotate-by i)
            (rotate n i))]
  (map rotate-by loi)))