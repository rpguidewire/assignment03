;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;**************************************
;;Assignment 3, Question 2
;;Shivangi Patwardhan 
;;**************************************

;;2A
(define-struct line (slope intercept))
;;A Line = (make-line Num Num)

;;2B
;;two-points->line: Posn Posn -> Line

(define (two-points->Line posn1 posn2)
  (cond
    [(= (- (posn-x posn1)
           (posn-x posn2)) 0) (make-line 'undefined (posn-x posn1))]
    [else (make-line (find-slope posn1 posn2) (- (posn-y posn1)
                                            (* (find-slope posn1 posn2) (posn-x posn1))))]))

(define (find-slope posn1 posn2)
  ( / (- (posn-y posn1)
         (posn-y posn2))
      (- (posn-x posn1)
         (posn-x posn2))))

;;2C
(define (perp-Line posn1 line1)
  (cond 
  [(= (line-slope line1)0) (make-line 'undefined (posn-x posn1)) ]
  [else (make-line (- (/ 1 (line-slope line1))) (- (posn-y posn1) (* (line-slope line1) (posn-x posn1))))]))


             