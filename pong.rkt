#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require rackunit)

;;simple two-player pong

;;================================
;;Constants:

(define WIDTH 1000)
(define HEIGHT 600)
(define BALL (circle 10 "solid" "white"))
(define PADDING (/ (image-width BALL) 2))
(define PADDLE (rectangle PADDING 75 "solid" "white"))
(define DIVIDER (square 0 "solid" "white"))
#;
(define DIVIDER
  (local ((define white-rectangle 
            (rectangle PADDING 40 "solid" "white"))
          (define black-rectangle 
            (rectangle PADDING 40 "solid" "black"))
          (define (divider-acc img acc)
            (cond ((>= (image-height img) HEIGHT) img)
                  (else
                   (if (= (modulo acc 2) 0)
                       (divider-acc (above white-rectangle img)
                                    (add1 acc))
                       (divider-acc (above black-rectangle img)
                                    (add1 acc)))))))
    (divider-acc black-rectangle 0)))
(define TICK-SPEED 0.015)
(define BALL-SPEED 5)
(define PADDLE-SPEED 10)
(define PADDLE-FACTOR 1.25)
(define MTS (empty-scene WIDTH HEIGHT "black"))

;;================================
;;Data definitions:

(struct element (pos vel speed))
;;Element is (element Posn Posn Integer)
;;Assume:
;;- Vel Posn -1 -> 1

(struct world (ball paddle1 paddle2))
;;World is (world Element Element Element Posn)
;;Assume:
;;- Paddle1 && Paddle2 Pos X fixed
;;- Paddle1 && Paddle2 Speed fixed
;;- Paddle1 && Paddle2 Vel -1 || 1

;;================================
;;Functions:

;;World -> World
(define (main w)
  (big-bang w
            (name "Pong")
            (on-tick tock TICK-SPEED)
            (to-draw render)
            (on-key handle-key)
            (on-release handle-release)))

;;================
;;Tock:

;;World -> World
(define (tock w)
  (world (map (move-ball (choose-paddle w))
              (world-ball w))
         (move-paddle (world-paddle1 w))
         (move-paddle (world-paddle2 w))))

;;World -> Element
(define (choose-paddle w)
  (if (>= (/ WIDTH 2) (posn-x (element-pos (world-ball w))))
      (world-paddle1 w)
      (world-paddle2 w)))

;;Element Element -> Element
(define (move-ball p)
  (lambda (b)
    (cond ((or (<= (posn-x (element-pos b))
                   (* 3 PADDING))
               (>= (posn-x (element-pos b))
                   (- WIDTH (* 3 PADDING))))
           (left/right-ball b p))
          ((or (>= (posn-y (element-pos b))
                   (- HEIGHT PADDING))
               (<= (posn-y (element-pos b))
                   PADDING))
           (up/down-ball b))
          (else
           (advance-element b)))))

;;Element Element -> Element
(define (left/right-ball b p)
  (define (bounce-factor p)
    (if (zero? (posn-y (element-vel p))) 1 2))
  (if (and (>= (posn-y (element-pos b))
               (- (posn-y (element-pos p))
                  (/ (image-height PADDLE) 2) PADDING))
           (<= (posn-y (element-pos b))
               (+ (posn-y (element-pos p))
                  (/ (image-height PADDLE) 2) PADDING)))
      (advance-element
       (element (element-pos b)
                (make-posn (- 0 (posn-x (element-vel b)))
                           (* (bounce-factor p) 
                              (posn-y (element-vel b))))
                (* PADDLE-FACTOR (element-speed b))))
      (element (make-posn (/ WIDTH 2)
                          (+ (/ HEIGHT 4) (/ (random HEIGHT) 2)))
               (reset-vel p)
               BALL-SPEED)))

;;Element -> Posn
(define (reset-vel p)
  (local ((define rand (* (/ pi 2) (/ (random 1000) 2000)))
          (define x (cos rand))
          (define y (sin rand)))
    (if (<= (/ WIDTH 2) (posn-x (element-pos p)))
        (make-posn x (- 0 y))
        (make-posn (- 0 x) y))))

;;Element -> Element
(define (up/down-ball b)
  (advance-element
   (element (element-pos b)
            (make-posn (posn-x (element-vel b))
                       (- 0 (posn-y (element-vel b))))
            (element-speed b))))

;;Element -> Element
(define (advance-element e)
  (element (make-posn (+ (posn-x (element-pos e))
                         (* (element-speed e)
                            (posn-x (element-vel e))))
                      (- (posn-y (element-pos e))
                         (* (element-speed e)
                            (posn-y (element-vel e)))))
           (element-vel e)
           (element-speed e)))

;;Element -> Element
(define (move-paddle p)
  (if (and (>= (- HEIGHT (/ (image-height PADDLE) 2))
               (posn-y (element-pos p)))
           (<= (/ (image-height PADDLE) 2)
               (posn-y (element-pos p))))
      (advance-element p)
      (advance-element
       (element (element-pos p)
                (make-posn (posn-x (element-vel p))
                           (- 0 (* 2 (posn-y (element-vel p)))))
                (element-speed p)))))

;;================
;;Render:

;;World -> Image
(define (render w)
  (define (choose-image e)
    (if (zero? (posn-x (element-vel e))) PADDLE BALL))
  (place-images (generate-list (world-ball w)
                               (world-paddle1 w)
                               (world-paddle2 w)
                               choose-image)
                (generate-list (world-ball w)
                               (world-paddle1 w)
                               (world-paddle2 w)
                               element-pos)
                (overlay DIVIDER MTS)))

;;World (listof Ball) Paddle Paddle (Element -> X) -> (listof X)
(define (generate-list i s1 s2 f)
  (define (generate-list-aux i s1 s2 f rsf)
    (if (empty? i)
        (append rsf (list (f s1) (f s2)))
        (generate-list-aux (rest i) s1 s2 f
                           (cons (f (first i)) rsf))))
  (generate-list-aux i s1 s2 f empty))

;;================
;;Keyboard:

;;World KeyEvent -> World
(define (handle-key w ke)
  (cond ((or (key=? ke "w") (key=? ke "s"))
         (world (world-ball w)
                (set-paddle ke (world-paddle1 w))
                (world-paddle2 w)))
        ((or (key=? ke "up") (key=? ke "down"))
         (world (world-ball w)
                (world-paddle1 w)
                (set-paddle ke (world-paddle2 w))))
        (else w)))

;;KeyEvent Paddle -> Paddle
(define (set-paddle ke p)
  (if (or (key=? ke "w") (key=? ke "up"))
      (element (element-pos p)
               (make-posn 0 1)
               (element-speed p))
      (element (element-pos p)
               (make-posn 0 -1)
               (element-speed p))))

;;World KeyEvent -> World
(define (handle-release w ke)
  (cond ((or (key=? ke "w") (key=? ke "s"))
         (world (world-ball w)
                (reset-paddle ke (world-paddle1 w))
                (world-paddle2 w)))
        ((or (key=? ke "up") (key=? ke "down"))
         (world (world-ball w)
                (world-paddle1 w)
                (reset-paddle ke (world-paddle2 w))))
        (else w)))

;;KeyEvent Paddle -> Paddle
(define (reset-paddle ke p)
  (if (or (key=? ke "w") (key=? ke "up"))
      (element (element-pos p)
               (make-posn 0 0)
               (element-speed p))
      (element (element-pos p)
               (make-posn 0 0)
               (element-speed p))))

;;================================
;;Run:

(main
 (world
  (list (element (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                 (make-posn 0.701 -0.701)
                 BALL-SPEED) empty)
  (element (make-posn (* (/ 3 2) PADDING)
                      (/ HEIGHT 2))
           (make-posn 0 0)
           PADDLE-SPEED)
  (element (make-posn (- WIDTH
                         (* (/ 3 2) PADDING))
                      (/ HEIGHT 2))
           (make-posn 0 0)
           PADDLE-SPEED)))
