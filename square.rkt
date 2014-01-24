;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname square) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; parallel? (two sets)
;; Distance between two lines  
;; perpendicular (opp. two sets)
;;issquare? 

(define-struct line (slope intercept))
;;A Line: Num Posn 

;;Helper Function 1
;;isparallel?: Line Line -> Boolean 

;;The purpose of this function is to take in two lines, line1 and line2 and conclude weather or not 
;; they are parallel 

;;Examples 
(check-expect (isparallel? (make-line 3 (make-posn 0 0)) (make-line -3 (make-posn 0 0))) false)
(check-expect (isparallel? (make-line 2/3 (make-posn 0 12)) (make-line 2/3 (make-posn 0 10))) true)

;;Function Definition 
(define (isparallel? line1 line2)
  (cond 
    [(and (equal? (line-slope line1) (line-slope line2))
          (or 
          (not (equal? (posn-x (line-intercept line1)) (posn-x (line-intercept line2))))
          (not (equal? (posn-y (line-intercept line1)) (posn-y (line-intercept line2)))))) true]
    [else false]))

;;Tests           
(check-expect (isparallel? (make-line -2/3 (make-posn 0 12)) (make-line 2/3 (make-posn 0 12))) false)
(check-expect (isparallel? (make-line -4/5 (make-posn 0 2)) (make-line -5/4 (make-posn 0 12))) false)
(check-expect (isparallel? (make-line -3 (make-posn 0 0)) (make-line -3 (make-posn 0 0))) false)
(check-expect (isparallel? (make-line 3 (make-posn 0 -2)) (make-line 3 (make-posn 0 10))) true)
(check-expect (isparallel? (make-line 1/3 (make-posn 0 1)) (make-line 1/3 (make-posn 0 10))) true)
(check-expect (isparallel? (make-line 2/5 (make-posn 0 -2)) (make-line 2/5 (make-posn 0 1))) true)
(check-expect (isparallel? (make-line 'undefined (make-posn 12 0)) 
                           (make-line 'undefined (make-posn 12 0))) false)
(check-expect (isparallel? (make-line 'undefined (make-posn -2 0)) 
                           (make-line 'undefined (make-posn 4 0))) true)

;;Helper Function 2
;;isperp?: Line Line --> boolean 

;; The purpose of this function is to take in two lines, line1 and line2 and 
;; check if they are perpendicular 

;; Examples 
(check-expect (isperp? (make-line 3 (make-posn 0 0)) (make-line -3 (make-posn 0 0))) false)
(check-expect (isperp? (make-line 1/3 (make-posn 0 12)) (make-line -3 (make-posn 0 10))) true)

;;Function Definition 
(define (isperp? line1 line2)
  (cond 
    [(or (equal? 'undefined (line-slope line1))
         (equal? 'undefined (line-slope line2)))
     (cond 
      [(and 
        (or (equal? 0 (line-slope line1))
            (equal? 0 (line-slope line2)))
        (or 
          (not (equal? (posn-x (line-intercept line1)) (posn-x (line-intercept line2))))
          (not (equal? (posn-y (line-intercept line1)) (posn-y (line-intercept line2))))))true]
      [else false])]
    [(equal? (line-slope line1)
            (/ -1 (line-slope line2))) true]
    [else false]))


(check-expect (isperp? (make-line -2/3 (make-posn 0 12)) (make-line 2/3 (make-posn 0 12))) false)
(check-expect (isperp? (make-line -4/5 (make-posn 0 2)) (make-line 5/4 (make-posn 0 12))) true)
(check-expect (isperp? (make-line -4/5 (make-posn 0 2)) (make-line -5/4 (make-posn 0 12))) false)
(check-expect (isperp? (make-line -3 (make-posn 0 0)) (make-line 1/3 (make-posn 0 0))) true)
(check-expect (isperp? (make-line 3 (make-posn 0 -2)) (make-line 3 (make-posn 0 10))) false)
(check-expect (isperp? (make-line 'undefined (make-posn 12 0))(make-line 0 (make-posn 0 3))) true)
(check-expect (isperp? (make-line 'undefined (make-posn -2 0)) (make-line 0 (make-posn 0 4))) true)

;;Helper Function 3: Distance
;;distance: Line Line --> Boolean 

;;The purpose of this function is to take in two parallel lines, LineA and LineB and 
;; and find the distance between the two lines. 

;;Examples 
(check-within (distance (make-line -2/3 (make-posn 0 12)) 
                        (make-line 2/3 (make-posn 0 2))) #i8.320502943378436 1/10000000 )

;;Function Definition 
(define (distance lineA lineB)
  (cond 
   [(or 
     (equal? 'undefined (line-slope lineA))
     (equal? 'undefined (line-slope lineB)))
    (abs (- (posn-x (line-intercept lineA)) (posn-x (line-intercept lineB))))]
   [else (/ (abs (- (posn-y (line-intercept lineA)) (posn-y (line-intercept lineB))))
            (sqrt ( + (sqr (line-slope lineA)) 1)))]))

;;Tests 
(check-expect (isparallel? (make-line 1/3 (make-posn 0 1)) (make-line 1/3 (make-posn 0 10))) true)
(check-expect (isparallel? (make-line 2/5 (make-posn 0 -2)) (make-line 2/5 (make-posn 0 1))) true)

;;square? Line Line Line Line --> Boolean 

;;The Purpose of this function is to take in four lines: LineA, LineB, LineC and LineD and confirm 
;; that all intersect in four distinct points to create a square. 

;;Function Definition 
(define (square? lineA lineB lineC lineD)
  (cond
    [(and (isparallel? lineA lineB)
          (isperp? lineA lineD)
          (isperp? lineA lineC)
          (equal? (distance lineA lineB)(distance lineC lineD))) true]
    
    [(and (isparallel? lineA lineC)
          (isperp? lineA lineB)
          (isperp? lineA lineD)
          (equal? (distance lineA lineC)(distance lineB lineD))) true]
    
    [(and (isparallel? lineA lineD)
          (isperp? lineA lineB)
          (isperp? lineA lineC)
          (equal? (distance lineA lineD)(distance lineB lineC))) true]
    
    [(and (isparallel? lineB lineC)
          (isperp? lineB lineA)
          (isperp? lineB lineD)
          (equal? (distance lineB lineC)(distance lineA lineD))) true]
    
    [(and (isparallel? lineB lineD)
          (isperp? lineB lineA)
          (isperp? lineB lineC)
          (equal? (distance lineB lineD)(distance lineA lineC))) true]
    
    [(and (isparallel? lineC lineD)
          (isperp? lineC lineA)
          (isperp? lineC lineB)
          (equal? (distance lineC lineD)(distance lineA lineB))) true]
    [else false]))


(check-expect (square? (make-line 'undefined (make-posn 0 -4)) (make-line 'undefined (make-posn 0 4))
                       (make-line 0 (make-posn -4 0)) (make-line 0 (make-posn 4 0))) true)

(check-expect (square? (make-line 1 (make-posn 0 2)) (make-line 1 (make-posn 0 -2))
                       (make-line -1 (make-posn -4 -2)) (make-line -1 (make-posn 4 2))) true)






























