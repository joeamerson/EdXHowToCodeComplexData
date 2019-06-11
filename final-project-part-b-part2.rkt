;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final-project-part-b-part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(define-struct slot (no grad?))
;; Slot is (make-slot Natural Boolean)
;; interp. slot has a time and whether it is a grading slot
;; all slots are same length and none overlap


(define-struct ta (name max avail grad?))

(define SOBA (make-ta "Soba" 2 (list 1 3) false))
(define UDON (make-ta "Udon" 1 (list 3 4) false))
(define RAMEN (make-ta "Ramen" 1 (list 2) false))

(define NOODLE-TAs (list SOBA UDON RAMEN))

(define TA1 (make-ta "Harry"    1 (list 1   3) false))
(define TA2 (make-ta "Ron"      1 (list   2 3) true))
(define TA3 (make-ta "Hermione" 2 (list   2    4) false))


(define TAS (list TA1 TA2 TA3))


(define SLOTS0 (list
                (make-slot 1 false)
                (make-slot 2 false)
                (make-slot 3 true)
                (make-slot 4 true)))


(define ERIKA (make-ta "Erika" 1 (list 1 3 7 9) false))
(define RYAN (make-ta "Ryan" 1 (list 1 8 10) true))
(define REECE (make-ta "Reece" 1 (list 5 6) false))
(define GORDON (make-ta "Gordon" 2 (list 2 3 9) false))
(define DAVID (make-ta "David" 2 (list 2 8 9) false))
(define KATIE (make-ta "Katie" 1 (list 4 6) true))
(define AASHISH (make-ta "Aashish" 2 (list 1 10) false))
(define GRANT (make-ta "Grant" 2 (list 1 11) false))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12) false))
(define ERIN (make-ta "Erin" 1 (list 4) false))

(define TA-LIST (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ERIN))

(define SLOTS-PART1 (list (make-slot 1 false)
                          (make-slot 2 false)
                          (make-slot 3 false)
                          (make-slot 4 false)))

(define SLOTS1 (list (make-slot 1 false)
                     (make-slot 2 true)
                     (make-slot 3 false)
                     (make-slot 4 true)
                     (make-slot 5 false)
                     (make-slot 6 true)
                     (make-slot 7 false)
                     (make-slot 8 true)
                     (make-slot 9 true)
                     (make-slot 10 true)
                     (make-slot 11 true)
                     (make-slot 12 false)))



;; TA is (make-ta String Natural (listof Slot) Boolean)
;; name, max hours, slots available, and whether they can only be grading TAs


(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)

;; AC is a (make-ac TA Natural)
;; interp. an assignment counter that keeps track of how many assignments a TA has been given so far
(define-struct ac (ta num))

;; ========================================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce a schedule for a given set of TAs and slots; false if impossible

(check-expect (schedule-tas-TR empty      empty)    empty)
(check-expect (schedule-tas-TR (list TA1) empty)    empty)
(check-expect (schedule-tas-TR empty (list (make-slot 1 false))) false)

(check-expect (schedule-tas-TR (list TA1) (list (make-slot 1 false))) (list
                                                             (make-assignment TA1 (make-slot 1 false))))
(check-expect (schedule-tas-TR (list TA1) (list (make-slot 1 true))) (list
                                                             (make-assignment TA1 (make-slot 1 true))))
(check-expect (schedule-tas-TR (list TA2) (list (make-slot 2 false))) false)
(check-expect (schedule-tas-TR (list TA2) (list (make-slot 2 true))) (list
                                                            (make-assignment TA2 (make-slot 2 true))))
(check-expect (schedule-tas-TR (list TA2) (list (make-slot 2 true) (make-slot 3 true))) false) 
 
(check-expect (schedule-tas-TR TAS SLOTS0) (list
                                  (make-assignment TA3 (make-slot 4 true))
                                  (make-assignment TA2 (make-slot 3 true))
                                  (make-assignment TA3 (make-slot 2 false))
                                  (make-assignment TA1 (make-slot 1 false))))

(check-expect (schedule-tas-TR TAS (list
                          (make-slot 4 true)
                          (make-slot 3 false)
                          (make-slot 2 true)
                          (make-slot 1 false))) false)

(check-expect (schedule-tas-TR TA-LIST SLOTS1) (list
                                      (make-assignment RAEANNE (make-slot 12 false))
                                      (make-assignment GRANT (make-slot 11 true))
                                      (make-assignment AASHISH (make-slot 10 true))
                                      (make-assignment DAVID (make-slot 9 true))
                                      (make-assignment RYAN (make-slot 8 true))
                                      (make-assignment ERIKA (make-slot 7 false))
                                      (make-assignment KATIE (make-slot 6 true))
                                      (make-assignment REECE (make-slot 5 false))
                                      (make-assignment ERIN (make-slot 4 true))
                                      (make-assignment GORDON (make-slot 3 false))
                                      (make-assignment GORDON (make-slot 2 true))
                                      (make-assignment AASHISH (make-slot 1 false))))

(check-expect (schedule-tas-TR (remove ERIKA TA-LIST) SLOTS1) false)
(check-expect (schedule-tas-TR TA-LIST empty) empty)


;;(define (schedule-tas-TR tas slots) false) ;stub
;; Cross product of type comments table
;;                      TAs ->      empty         (cons TA (listof TA))
;;  Slots
;;   |
;;  empty                           empty (1)                 empty (1)
;;  (cons Natural (listof Natural)) false (2)        Try to match TAs to slots (3)

(define (schedule-tas-TR tas slots)
  (cond [(empty? slots) empty] ;1
        [(empty? tas) false]   ;2
        [else                  ;3
         (schedule-solve-TR tas slots)]))

;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible
;; Goes thru slots 1 by 1 to produce next schedules
;;(define (schedule-solve tas slots) empty);STUB

(define (schedule-solve-TR tas0 slots0)
  ;; added todo: a list of schedules that need to be "checked" so far
  (local [(define (solve--sched tas slots schedule todo)
            (cond [(empty? slots) false]
                  [(empty? schedule) (solve--los tas slots (append (next-valid-schedules-TR tas slots schedule)
                                                                   todo))]
                  [(solved? tas0 slots0 schedule) schedule]
                  [else
                   (solve--los tas (rest slots) (append (next-valid-schedules-TR tas (rest slots) schedule)
                                                        todo))]))
          (define (solve--los tas slots todo)
            (cond [(empty? slots) false]
                  [(empty? todo) false]
                  [else
                   (local [(define try
                             (solve--sched tas slots (first todo) (rest todo)))]
                     (if (not (false? try))
                         try
                         (solve--los tas slots (rest todo))))]))]
    (solve--sched tas0 slots0 empty empty)))


;; (listof TA) (listof Slot) (listof Assignment) -> (listof Schedule) | false
;; Creates the next set of schedules with the next first slot filled in with all TA options (whether valid or not)
;;(define (next-schedules tas slots schedule) empty);stub

(check-expect (next-schedules NOODLE-TAs SLOTS-PART1 empty)
              (list (list
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment UDON (make-slot 1 false)))
                    (list
                     (make-assignment RAMEN (make-slot 1 false)))))
(check-expect (next-schedules NOODLE-TAs (list (make-slot 2 false)
                                               (make-slot 3 false)
                                               (make-slot 4 false))
                              (list
                               (make-assignment SOBA (make-slot 1 false))))
              (list (list
                     (make-assignment SOBA (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment UDON (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))))
(check-expect (next-schedules  NOODLE-TAs (list (make-slot 3 false)
                                                (make-slot 4 false))
                               (list
                                (make-assignment RAMEN (make-slot 2 false))
                                (make-assignment SOBA (make-slot 1 false))))
              (list (list
                     (make-assignment SOBA (make-slot 3 false))
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment UDON (make-slot 3 false))
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment RAMEN (make-slot 3 false))
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))))

(define (next-schedules tas slots schedule)
  (local [(define (add-assignment-to-front t)
            (cons (make-assignment t (first slots)) schedule))]
    (if (empty? slots)
        false
        (map add-assignment-to-front tas))))


;; (listof TA) (listof Slot) (listof Assignment) -> (listof Schedule)
;; filter out invalid schedules from next-schedules (i.e. TA is overbooked, TA is scheduled for a slot they are not available)
(check-expect (next-valid-schedules-TR NOODLE-TAs SLOTS-PART1 empty)
              (list (list
                     (make-assignment SOBA (make-slot 1 false)))))
(check-expect (next-valid-schedules-TR NOODLE-TAs (list (make-slot 2 false)
                                                     (make-slot 3 false)
                                                     (make-slot 4 false))
                                    (list
                                     (make-assignment SOBA (make-slot 1 false))))
              (list (list
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))))
(check-expect (next-valid-schedules-TR NOODLE-TAs (list
                                                (make-slot 3 false)
                                                (make-slot 4 false))
                                    (list
                                     (make-assignment RAMEN (make-slot 2 false))
                                     (make-assignment SOBA (make-slot 1 false))))
              (list (list
                     (make-assignment SOBA (make-slot 3 false))
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))
                    (list
                     (make-assignment UDON (make-slot 3 false))
                     (make-assignment RAMEN (make-slot 2 false))
                     (make-assignment SOBA (make-slot 1 false)))))
(check-expect (next-valid-schedules-TR (list ERIKA RYAN REECE)
                                    (list (make-slot 8 true))
                                    (list (make-assignment ERIKA (make-slot 1 false))))
              (list (list
                     (make-assignment RYAN (make-slot 8 true))
                     (make-assignment ERIKA (make-slot 1 false)))))
(check-expect (next-valid-schedules-TR (list ERIKA RYAN REECE)
                                    (list (make-slot 8 false))
                                    (list (list (make-assignment ERIKA (make-slot 1 false)))))
              empty)
(check-expect (next-valid-schedules-TR (list ERIKA RYAN REECE)
                                    (list (make-slot 5 false))
                                    (list (make-assignment ERIKA (make-slot 1 false))))
              (list (list (make-assignment REECE (make-slot 5 false))
                          (make-assignment ERIKA (make-slot 1 false)))))
(check-expect (next-valid-schedules-TR (list ERIKA RYAN REECE)
                                    (list (make-slot 5 true))
                                    (list (make-assignment ERIKA (make-slot 1 false))))
              (list (list (make-assignment REECE (make-slot 5 true))
                          (make-assignment ERIKA (make-slot 1 false)))))


(define (next-valid-schedules-TR tas slots schedule)
  (local [(define (not-overbooked? schedule)    ;; Schedule -> Boolean
                                                ;; Checks to see if anyone in the schedule has too many assignments for their max avail
            ;; added rsf: accumulator to count result so far in the form of (make-ac ta num)
            (local [(define (not-overbooked? tas schedule rsf)
                      (cond [(empty? schedule) (ta-not-overbooked? rsf)]
                            [else
                             (not-overbooked? tas (rest schedule) (merge-ta (first schedule) rsf))]))]
              (not-overbooked? tas schedule empty)))
          (define (merge-ta a rsf)              ;; Assignment (listof AC) -> (listof AC)
                                                ;; Updates the result-so-far based on the assigment given
            (cond [(empty? rsf) (list (make-ac (assignment-ta a) 1))]
                  [else
                   (if (string=? (ta-name (assignment-ta a))
                                 (ta-name (ac-ta (first rsf))))
                       (cons (update-ta-assignment-counter (first rsf)) (rest rsf))
                       (cons (first rsf) (merge-ta a (rest rsf))))]))
          (define (update-ta-assignment-counter ac)          ;; AC -> Boolean
            (make-ac (ac-ta ac) (add1 (ac-num ac))))         ;; Adds 1 to assignment counter
          (define (ta-not-overbooked? rsf)                   ;; (listof AC) -> Boolean
            (local [(define (not-overbooked? ac)
                      (<= (ac-num ac) (ta-max (ac-ta ac))))] ;; Returns true as long as assignment counter is not over the TA's max
              (andmap not-overbooked? rsf)))
          (define (valid? schedule)                          ;; Schedule -> Boolean
            (local [(define (bad-assignment? a)              ;; Returns true only if there's an invalid assignment (i.e. TA not available for slot or TA not on-campus for campus-required slot)
                      (or (not (member (slot-no (assignment-slot a)) (ta-avail (assignment-ta a)))) ;;if the slot isn't in the TA's availability, then it's a bad assignment!
                          (off-campus-problem? a)))
                    (define (off-campus-problem? a)                   ;; Assignment -> Boolean
                      (and (false? (slot-grad? (assignment-slot a)))  ;; Returns true only if the TA is off-campus (ta-grad? true) and the Slot is on-campus (slot-grad? false)
                           (not (false? (ta-grad? (assignment-ta a))))))] 
              (not (ormap bad-assignment? schedule))))
          (define try
            (next-schedules tas slots schedule))]
    (if (false? try)
        empty
        (filter not-overbooked?
                (filter valid? try)))))


;; (listof TA) (listof Slot) (listof Assignment) -> Boolean
;; Assume: only valid solutions have been provided (since next-valid-schedules returns only valid schedules)
;;         so we only need to make sure the # of assignments matches the # of slots
;; Assume: TAs & Slots will not be empty
;;(define (solved? tas slots schedule) false);stub
(check-expect (solved? NOODLE-TAs (list 1 2 3 4 5) empty) false)
(check-expect (solved? NOODLE-TAs (list 1 2 3 4 5) (list
                                                    (make-assignment SOBA 1))) false)
(check-expect (solved? NOODLE-TAs (list 1 2 3 4) (list
                                                  (make-assignment UDON 4)
                                                  (make-assignment SOBA 3)
                                                  (make-assignment RAMEN 2)
                                                  (make-assignment SOBA 1))) true)
(check-expect (solved? NOODLE-TAs (list 1 2 3 4 5) (list
                                                    (make-assignment UDON 4)
                                                    (make-assignment SOBA 3)
                                                    (make-assignment RAMEN 2)

                                                    (make-assignment SOBA 1))) false)

(define (solved? tas slots schedule)
  (= (length slots) (length schedule)))
