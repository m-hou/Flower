;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define PETALSIZE 5)
(define PETALNUM 34)
(define GROWTHCONSTANT 1)


(define BMAX 218)
(define BMIN 112)
(define CD (- BMAX BMIN))
(define CD8 CD)
(define CD5 (* (/ CD 8) 5))
(define CD3 (* (/ CD 8) 3))
(define ANGLE 137.5)
(define WIDTH 1000)
(define HEIGHT 800)
(define MTS (empty-scene WIDTH HEIGHT))


(define (main ws)
  (big-bang ws                   ; WS
            (to-draw   render)   ; WS -> Image -> WS
            (on-key    handle-key)))    ; WS KeyEvent -> WS

(define (petal i x c)
  (local [(define (petals i x acc img)
            (local [(define petal (put-pinhole
                                   i i
                                   (overlay (ellipse (* i 5) (* i 2) "solid"
                                                     (cond [(= c 3) (colorpick3 (modulo acc c))]
                                                           [(= c 5) (colorpick5 (modulo acc c))]
                                                           [(= c 8) (colorpick8 (modulo acc c))]))
                                            
                                            (ellipse (* i 5) (* i 2) "outline" "Maroon"))))]
              (cond [(> acc x) img]
                    [else
                     (petals (+ i GROWTHCONSTANT)
                             x
                             (+ acc 1)
                             (underlay/pinhole (rotate (* ANGLE acc) petal) img))
                     ])))]
    (petals i x 1 empty-image)))

(define (colorpick3 num)
  (cond [(= num 0) (make-color (round (- BMAX CD3)) BMIN BMAX)]
        [(= num 1) (make-color BMAX BMIN BMAX)]
        [(= num 2) (make-color BMAX BMIN (round (- BMAX CD3)))]))

(define (colorpick5 num)
  (cond [(= num 0) (make-color (round (- BMAX CD5)) BMIN BMAX)]
        [(= num 1) (make-color (round (- BMAX (/ CD5 2))) BMIN BMAX)]
        [(= num 2) (make-color BMAX BMIN BMAX)]
        [(= num 3) (make-color BMAX BMIN (round (- BMAX (/ CD5 2))))]
        [(= num 4) (make-color BMAX BMIN (round (- BMAX CD5)))]))

(define (colorpick8 num)
  (cond [(= num 0) (make-color BMIN BMIN BMAX)]
        [(= num 1) (make-color (round (- BMAX (* (/ CD8 4) 3))) BMIN BMAX)]
        [(= num 2) (make-color (round (- BMAX (/ CD8 2))) BMIN BMAX)]
        [(= num 3) (make-color (round (- BMAX (/ CD8 4))) BMIN BMAX)]
        [(= num 4) (make-color BMAX BMIN (round (- BMAX (/ CD8 4))))]
        [(= num 5) (make-color BMAX BMIN (round (- BMAX (/ CD8 2))))]
        [(= num 6) (make-color BMAX BMIN (round (- BMAX (* (/ CD8 4) 3))))]
        [(= num 7) (make-color BMAX BMIN BMIN)]))

; i is initial size
; x is number of petals
; c is number for petals that are colored the same color
(define (flower i x c)
  (clear-pinhole (overlay/pinhole (circle (* i 2) "solid" "Gold") (petal i x c))))

(define (render ws)
  (overlay ws MTS))


(define (handle-key ws ke)
  (cond [(key=? ke "3") (flower PETALSIZE PETALNUM 3)]
        [(key=? ke "5") (flower PETALSIZE PETALNUM 5)]
        [(key=? ke "8") (flower PETALSIZE PETALNUM 8)]
        [else 
         ws]))

(main empty-image)