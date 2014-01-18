;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;****************************************
;;Assignment 03, Question 3
;;****************************************

;;3A

(define-struct card (suit rank))
;;A Card is (make-card (Symbol Int))

;;3B

;;Helper Function 
;; compare-suit: Symbol -> Boolean 

;;The Purpose of this function is to take in a symbol, a-suit-input and depending on 
;; what symbol it is, a number is returns, 1 for the worst and 4 for the best (in that order)

;;Examples 
(check-expect (compare-suit 'clubs) 1)
(check-expect (compare-suit 'diamonds) 2)

;;Helper Function 
(define (compare-suit a-suit-input)
  (cond
    [(symbol=? a-suit-input 'clubs) 1]
    [(symbol=? a-suit-input 'diamonds) 2]
    [(symbol=? a-suit-input 'hearts) 3]
    [(symbol=? a-suit-input 'spades) 4]))

;;Tests
(check-expect (compare-suit 'hearts) 3)
(check-expect (compare-suit 'spades) 4)


;;Main Function 
;;better-Card: Card Card -> Card

;;The purpose of this function is to take in two cards, card1 and card2, and return the
;; better card based on the set of rules presented 

;;Examples 
(check-expect (better-Card (make-card 'clubs 10)
                           (make-card 'clubs 1)) (make-card 'clubs 10))

(check-expect (better-Card (make-card 'spades 10)
                           (make-card 'spades 9)) (make-card 'spades 10))

;;Fucntion Definition 
(define (better-Card card1 card2)
  (cond 
    [(symbol=? (card-suit card1)
               (card-suit card2)) 
     (cond 
       [(> (card-rank card1) (card-rank card2)) card1]
       [else card2])]
    [else 
     (cond 
       [(> (compare-suit (card-suit card1))
           (compare-suit (card-suit card2))) card1]
       [else card2])]))

;;Tests 
(check-expect (better-Card (make-card 'hearts 13)
                           (make-card 'spades 1)) (make-card 'spades 1))


(check-expect (better-Card (make-card 'hearts 1)
                           (make-card 'spades 11)) (make-card 'spades 11))


(check-expect (better-Card (make-card 'diamonds 1)
                           (make-card 'hearts 1)) (make-card 'hearts 1))


(check-expect (better-Card (make-card 'diamonds 7)
                           (make-card 'clubs 1)) (make-card 'diamonds 7))


(check-expect (better-Card (make-card 'diamonds 7)
                           (make-card 'diamonds 7)) (make-card 'diamonds 7))

(check-expect (better-Card (make-card 'hearts 7)
                           (make-card 'clubs 2)) (make-card 'hearts 7))

;;3C

;;isinsequence?: Num Num Num -> Boolean 

;; The purposed of this function is to tell weather the three numbers x, y and z
;;are consecutive or not.

;;Examples

(check-expect (isinsequence? 0 0 0) false)
(check-expect (isinsequence? 0 1 2) true)

;;Helper Function 
(define (isinsequence? x y z)
  (cond 
   [(and  (= 1 (- z y)) (= 1 (- y x))) true]
   [(and (= 1 (- y z)) (= 1 (- z x))) true]
   [(and (= 1 (- z x)) (= 1 (- x y))) true]
   [(and (= 1 (- x z)) (= 1 (- z y))) true]
   [(and (= 1 (- x y)) (= 1 (- y z))) true]
   [(and (= 1 (- y x)) (= 1 (- x z))) true]
   [else false]))
    
;;Tests 


(check-expect (isinsequence? 4 6 5) true)
(check-expect (isinsequence? 6 2 2) false)
(check-expect (isinsequence? 9 10 8) true)
(check-expect (isinsequence? 6 7 5) true)
(check-expect (isinsequence? 6 5 7) true)
(check-expect (isinsequence? 5 4 6) true)
(check-expect (isinsequence? 4 5 6) true)
(check-expect (isinsequence? 2 5 6) false)
(check-expect (isinsequence? 2 3 7) false)

;;hand-value: Card Card Card -> Symbol 

;;The purpose of this function is to take in three cards: card1, card2 and card3 and to 
;; return the appropriate hand-value and listed in the instructions 

;;Examples 

    (check-expect (hand-value (make-card 'club 10) (make-card 'club 9) (make-card 'club 8)) 'straight-flush)
    
;; Function Definition 
(define (hand-value card1 card2 card3)
  (cond
    [(and (equal? (card-suit card1)
                  (card-suit card2))
          (equal? (card-suit card2)
                  (card-suit card3))
          (equal? (card-suit card1)
                  (card-suit card3))
          (isinsequence? (card-rank card1) (card-rank card2) (card-rank card3))) 'straight-flush]
    [(and (equal? (card-suit card1)
                  (card-suit card2))
          (equal? (card-suit card2)
                  (card-suit card3))
          (equal? (card-suit card1)
                  (card-suit card3))) 'flush]
    [(isinsequence? (card-rank card1) (card-rank card2) (card-rank card3)) 'straight]
    [(and (= (card-rank card1)
            (card-rank card2))
         (= (card-rank card2)
            (card-rank card3))
         (= (card-rank card1)
            (card-rank card3))) 'three-of-a-kind]
    [(or (= (card-rank card1)
            (card-rank card2))
         (= (card-rank card2)
            (card-rank card3))
         (= (card-rank card1)
            (card-rank card3))) 'pair]
    [else 'high-card]))

;;Tests
    (check-expect (hand-value (make-card 'club 10) (make-card 'club 9) (make-card 'club 8)) 'straight-flush)
    (check-expect (hand-value (make-card 'club 2) (make-card 'club 9) (make-card 'club 8)) 'flush)
    (check-expect (hand-value (make-card 'diamonds 2) (make-card 'spades 3) (make-card 'club 4)) 'straight)
    (check-expect (hand-value (make-card 'diamonds 3) (make-card 'spades 3) (make-card 'club 3)) 'three-of-a-kind)
    (check-expect (hand-value (make-card 'diamonds 3) (make-card 'spades 7) (make-card 'club 3)) 'pair)
    (check-expect (hand-value (make-card 'diamonds 5) (make-card 'spades 7) (make-card 'club 3)) 'high-card)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    