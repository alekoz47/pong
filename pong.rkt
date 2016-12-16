;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

;;simple two-player pong

;;================================
;;Constants:

(define WIDTH 1000)
(define HEIGHT 600)
(define BALL (circle 20 "solid" "white"))
(define PADDLE (rectangle (/ (image-width BALL) 4) 150 "solid" "white"))
(define PADDING (/ (image-width BALL) 2))
(define TICK-SPEED 0.015)
(define PADDLE-SPEED 40)
(define MTS (empty-scene WIDTH HEIGHT "black"))

;;================================
;;Data definitions:

(define-struct element (pos vel speed))
;;Element is (make-element Posn Posn Integer)
;;Assume:
;;- Vel Posn -1 -> 1

(define-struct world (ball paddle1 paddle2 score))
;;World is (make-world Element Element Element Posn)
;;Assume:
;;- Paddle1 && Paddle2 Pos X fixed
;;- Paddle1 && Paddle2 Speed fixed
;;- Paddle1 && Paddle2 Vel -1 || 1
;;- Score Posn X && Score Posn Y 0 -> 7

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

;;World -> World
(define (tock w)
  (make-world (move-ball (world-ball w)
                         (choose-paddle w))
              (world-paddle1 w)
              (world-paddle2 w)))

;;World -> Element
(define (choose-paddle w)
  (cond ((<= (/ WIDTH 2) (posn-x (element-pos (world-ball w))))
         (world-paddle1 w))
        (else
         (world-paddle2 w))))

;;Element Element -> Element
(define (move-ball b p)
  (cond ((or (<= (posn-x (ball-pos b))
                 (+ PADDING (image-width PADDLE)))
             (>= (posn-x (ball-pos b))
                 (- WIDTH (+ PADDING (image-width PADDLE)))))
         (left/right-ball b p))
        ((or (>= (posn-y (ball-pos b))
                 (- HEIGHT PADDING))
             (<= (posn-y (ball-pos b))
                 PADDING))
         (up/down-ball b))
        (else
         (advance-ball b))))

;;Element Element -> Element
;;!!!
(define (left/right-ball b p) b) ;stub

;;Element -> Element
;;!!!
(define (up/down-ball b) b) ;stub

;;Element -> Element
;;!!!
(define (advance-ball b) b) ;stub

;;World -> World String
;;bounces ball if on paddle, otherwise serve
(define (paddle-ball w)
  (cond ((and (< (pos-x (ball-pos (world-ball w))) (/ WIDTH 2))
              (>= (pos-y (ball-pos (world-ball w)))
                  (- (pos-y (paddle-pos (world-paddle1 w)))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (paddle-pos (world-paddle1 w)))
                     (/ (image-height PADDLE) 2))))
         (return w "left"))
        ((and (> (pos-x (ball-pos (world-ball w))) (/ WIDTH 2))
              (>= (pos-y (ball-pos (world-ball w)))
                  (- (pos-y (paddle-pos (world-paddle2 w)))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (paddle-pos (world-paddle2 w)))
                     (/ (image-height PADDLE) 2))))
         (return w "right"))
        (else (serve w))))

;;World -> World String
;;bounces ball off wall
(define (wall-ball w)
  (make-world
   (make-ball
    (ball-reset (world-ball w) "top/bottom")
    (make-vel (round-five (vel-x (ball-vel (world-ball w))))
              (- 0 (round-five (vel-y (ball-vel (world-ball w)))))))
   (world-paddle1 w)
   (world-paddle2 w)))

;;World String -> World
;;builds new worldstate after ball bounce, inverts velocity
(define (return w s)
  (make-world
   (make-ball
    (ball-reset (world-ball w) s)
    (make-vel (- 0 (round-five (vel-x (ball-vel (world-ball w)))))
              (round-five (vel-y (ball-vel (world-ball w))))))
   (world-paddle1 w)
   (world-paddle2 w)))

;;Pos String -> Pos
;;resets ball position
(define (ball-reset wb s)
  (cond ((or (string=? s "left") (string=? s "right"))
         (make-pos (- (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (+ (round-five (pos-y (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb)))))))
        ((string=? s "top/bottom")
         (make-pos (+ (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (+ (round-five (pos-y (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb)))))))))

;;World -> World
;;switches angle randomly
(define (serve w)
  (if (= (random 2) 1)
      (make-world (make-ball
                   (make-pos (/ WIDTH 2) (/ HEIGHT 2))
                   (make-vel (- 0 (/ 1 (sqrt 2))) (/ 1 (sqrt 2))))
                  (world-paddle1 w)
                  (world-paddle2 w))
      (make-world (make-ball
                   (make-pos (/ WIDTH 2) (/ HEIGHT 2))
                   (make-vel (/ 1 (sqrt 2)) (/ 1 (sqrt 2))))
                  (world-paddle1 w)
                  (world-paddle2 w))))

;;World -> World
;;advances ball position with ball velocity and speed
(define (move-ball w)
  (make-world (make-ball
               (make-pos
                (+ (round-five (pos-x (ball-pos (world-ball w))))
                   (* BALL-SPEED
                      (round-five (vel-x (ball-vel (world-ball w))))))
                (- (round-five (pos-y (ball-pos (world-ball w))))
                   (* BALL-SPEED
                      (round-five (vel-y (ball-vel (world-ball w)))))))
               (ball-vel (world-ball w)))
              (make-paddle
               (pos-x (paddle-pos (world-paddle1 w)))
               (- (pos-y (paddle-pos (world-paddle1 w)))
                  (* PADDLE-SPEED (vel-y (paddle-vel (world-paddle1 w))))))
              (make-paddle
               (pos-x (paddle-pos (world-paddle2 w)))
               (- (pos-y (paddle-pos (world-paddle2 w)))
                  (* PADDLE-SPEED (vel-y (paddle-vel (world-paddle2 w))))))))

;;Number -> Number
;;truncates inexact number to 5 decimal places
(define (round-five n)
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;World -> Image
;;render world on screen at positions
(define (render w)
  (place-images (list BALL PADDLE PADDLE)
                (list (make-posn
                       (pos-x (ball-pos (world-ball w)))
                       (pos-y (ball-pos (world-ball w))))
                      (make-posn
                       (pos-x (paddle-pos (world-paddle1 w)))
                       (pos-y (paddle-pos (world-paddle1 w))))
                      (make-posn
                       (pos-x (paddle-pos (world-paddle2 w)))
                       (pos-y (paddle-pos (world-paddle2 w)))))
                MTS))

;;World KeyEvent -> World
;;start moving paddle
(define (handle-key w ke)
  (cond ((key=? ke "w")
         (make-world (world-ball w)
                     (make-paddle
                      (paddle-pos (world-paddle1 w))
                      (make-vel 0 1))
                     (world-paddle2 w)))
        ((key=? ke "s")
         (make-world (world-ball w)
                     (make-paddle
                      (paddle-pos (world-paddle1 w))
                      (make-vel 0 -1))
                     (world-paddle2 w)))
        ((key=? ke "up")
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (make-paddle
                      (paddle-pos (world-paddle2 w))
                      (make-vel 0 1))))
        ((key=? ke "down")
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (make-paddle
                      (paddle-pos (world-paddle2 w))
                      (make-vel 0 -1))))
        (else w)))
#;
(define (handle-key-use w ke)
  (cond ((and (key=? ke "w")
              (>= (- (pos-y (world-paddle1 w))
                     (* 0.5 (image-height PADDLE))) 0))
         w)
        ((and (key=? ke "s")
              (<= (+ (pos-y (world-paddle1 w))
                     (* 0.5 (image-height PADDLE))) HEIGHT))
         w)
        ((and (key=? ke "up")
              (>= (- (pos-y (world-paddle2 w))
                     ( * 0.5 (image-height PADDLE))) 0))
         w)
        ((and (key=? ke "down")
              (<= (+ (pos-y (world-paddle2 w))
                     (* 0.5 (image-height PADDLE))) HEIGHT))
         w)
        (else w)))

;;World KeyEvent -> World
;;stop moving paddle
(define (handle-release w ke)
  (cond ((key=? ke "w")
         (make-world (world-ball w)
                     (make-paddle
                      (paddle-pos (world-paddle1 w))
                      (make-vel 0 0))
                     (world-paddle2 w)))
        ((key=? ke "s")
         (make-world (world-ball w)
                     (make-paddle
                      (paddle-pos (world-paddle1 w))
                      (make-vel 0 0))
                     (world-paddle2 w)))
        ((key=? ke "up")
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (make-paddle
                      (paddle-pos (world-paddle2 w))
                      (make-vel 0 0))))
        ((key=? ke "down")
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (make-paddle
                      (paddle-pos (world-paddle2 w))
                      (make-vel 0 0))))
        (else w)))

;;================================
;;Run

(main
 (make-world
  (make-ball (make-pos (/ WIDTH 2) (/ HEIGHT 2))
             (make-vel (- 0 (/ 1 (sqrt 2))) (- 0 (/ 1 (sqrt 2)))))
  (make-paddle (make-pos (/ (image-width PADDLE) 2) (/ HEIGHT 2))
               (make-vel 0 0))
  (make-paddle (make-pos (- WIDTH (/ (image-width PADDLE) 2)) (/ HEIGHT 2))
               (make-vel 0 0))))
