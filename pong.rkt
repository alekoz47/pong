;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;simple two-player pong

;;================
;;Constants:

(define WIDTH 1000)
(define HEIGHT 600)
(define BALL (circle 20 "solid" "white"))
(define PADDLE (rectangle (/ (image-width BALL) 2) 150 "solid" "white"))
(define PADDING (/ (image-width BALL) 2))
(define TICK-SPEED 0.015)
(define PADDLE-SPEED 40)
(define BALL-SPEED 10)
(define MTS (empty-scene WIDTH HEIGHT "black"))

;;================
;;Data definitions:

(define-struct pos (x y))
;;Position is (make-pos Number[0, (- WIDTH PADDING)]
;;                      Number[0, (- HEIGHT PADDING)])
#;
(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

(define-struct vel (x y))
;;Velocity is (make-vel Number[-1, 1] Number[-1, 1])
#;
(define (fn-for-vel v)
  (... (vel-x v)
       (vel-y v)))

(define-struct ball (pos vel))
;;Ball is (make-ball Position Velocity)
#;
(define (fn-for-ball b)
  (... (fn-for-pos (ball-pos b))
       (fn-for-vel (ball-vel b))))

(define-struct world (ball paddle1 paddle2))
;;World is (make-world Ball Pos Pos)
#;
(define (fn-for-world w)
  (... (fn-for-ball (world-ball w))
       (fn-for-pos (world-paddle1 w))
       (fn-for-pos (world-paddle2 w))))

;;================
;;Functions:

;;World -> World
;;start world with (main ...)
(define [main w]
  (big-bang w
            (name "Simple Pong")
            (on-tick tock TICK-SPEED)
            (to-draw render)
            (on-key handle-key)))

;;World -> World
;;decides if ball is left/right, up/down, or between
(define [tock w]
  (cond [(or (<= (round-five (pos-x (ball-pos (world-ball w))))
                 (+ PADDING (image-width PADDLE)))
             (>= (round-five (pos-x (ball-pos (world-ball w))))
                 (- WIDTH (+ PADDING (image-width PADDLE)))))
         (paddle-ball w)]
        [(or (>= (round-five (pos-y (ball-pos (world-ball w))))
                 (- HEIGHT PADDING))
             (<= (round-five (pos-y (ball-pos (world-ball w))))
                 PADDING))
         (wall-ball w)]
        [else (move-ball w)]))

;;World -> World String
;;bounces ball if on paddle, otherwise serve
(define [paddle-ball w]
  (cond [(and (< (pos-x (ball-pos (world-ball w))) (/ WIDTH 2))
              (>= (pos-y (ball-pos (world-ball w)))
                  (- (pos-y (world-paddle1 w))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (world-paddle1 w))
                     (/ (image-height PADDLE) 2))))
         (return w "left/right")]
        [(and (> (pos-x (ball-pos (world-ball w))) (/ WIDTH 2))
              (>= (pos-y (ball-pos (world-ball w)))
                  (- (pos-y (world-paddle2 w))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (world-paddle2 w))
                     (/ (image-height PADDLE) 2))))
         (return w "left/right")]
        [else (serve w)]))

;;World String -> World
;;builds new worldstate after ball bounce, inverts velocity
(define [return w s]
  (make-world
   (make-ball
    (ball-reset (world-ball w) s)
    (make-vel (- 0 (round-five (vel-x (ball-vel (world-ball w)))))
              (round-five (vel-y (ball-vel (world-ball w))))))
   (world-paddle1 w)
   (world-paddle2 w)))

;;Pos String -> Pos
;;resets ball position
(define [ball-reset wb s]
  (cond [(string=? s "left/right")
         (make-pos (- (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (+ (round-five (pos-y (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb))))))]
        [(string=? s "top/bottom")
         (make-pos (+ (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (+ (round-five (pos-y (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb))))))]))

;;World -> World
;;switches angle randomly
(define [serve w]
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

;;World -> World String
;;bounces ball off wall
(define [wall-ball w]
  (make-world
   (make-ball
    (ball-reset (world-ball w) "top/bottom")
    (make-vel (round-five (vel-x (ball-vel (world-ball w))))
              (- 0 (round-five (vel-y (ball-vel (world-ball w)))))))
   (world-paddle1 w)
   (world-paddle2 w)))

;;World -> World
;;advances ball position with ball velocity and speed
(define [move-ball w]
  (make-world (make-ball
               (make-pos
                (+ (round-five (pos-x (ball-pos (world-ball w))))
                   (* BALL-SPEED
                      (round-five (vel-x (ball-vel (world-ball w))))))
                (- (round-five (pos-y (ball-pos (world-ball w))))
                   (* BALL-SPEED
                      (round-five (vel-y (ball-vel (world-ball w)))))))
               (ball-vel (world-ball w)))
              (world-paddle1 w)
              (world-paddle2 w)))

;;Number -> Number
;;truncates inexact number to 5 decimal places
(define [round-five n]
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;World -> Image
;;render world on screen at positions
(define [render w]
  (place-images (list BALL PADDLE PADDLE)
                (list (make-posn
                       (pos-x (ball-pos (world-ball w)))
                       (pos-y (ball-pos (world-ball w))))
                      (make-posn
                       (pos-x (world-paddle1 w))
                       (pos-y (world-paddle1 w)))
                      (make-posn
                       (pos-x (world-paddle2 w))
                       (pos-y (world-paddle2 w))))
                MTS))

;;World KeyEvent -> World
;;w, up: paddle up 5, s, down: paddle down 5
(define [handle-key w ke]
  (cond [(and (key=? ke "w")
              (>= (- (pos-y (world-paddle1 w))
                     (* 0.5 (image-height PADDLE))) 0))
         (make-world
          (world-ball w)
          (make-pos
           (pos-x (world-paddle1 w))
           (- (pos-y (world-paddle1 w)) PADDLE-SPEED))
          (world-paddle2 w))]
        [(and (key=? ke "s")
              (<= (+ (pos-y (world-paddle1 w))
                     (* 0.5 (image-height PADDLE))) HEIGHT))
         (make-world
          (world-ball w)
          (make-pos
           (pos-x (world-paddle1 w))
           (+ (pos-y (world-paddle1 w)) PADDLE-SPEED))
          (world-paddle2 w))]
        [(and (key=? ke "up")
              (>= (- (pos-y (world-paddle2 w))
                     ( * 0.5 (image-height PADDLE))) 0))
         (make-world
          (world-ball w)
          (world-paddle1 w)
          (make-pos
           (pos-x (world-paddle2 w))
           (- (pos-y (world-paddle2 w)) PADDLE-SPEED)))]
        [(and (key=? ke "down")
              (<= (+ (pos-y (world-paddle2 w))
                     (* 0.5 (image-height PADDLE))) HEIGHT))
         (make-world
          (world-ball w)
          (world-paddle1 w)
          (make-pos
           (pos-x (world-paddle2 w))
           (+ (pos-y (world-paddle2 w)) PADDLE-SPEED)))]
        [else w]))

;;================
;;Run

(main
 (make-world
  (make-ball (make-pos (/ WIDTH 2) (/ HEIGHT 2))
             (make-vel (- 0 (/ 1 (sqrt 2))) (- 0 (/ 1 (sqrt 2)))))
  (make-pos (/ (image-width PADDLE) 2) 300)
  (make-pos (- WIDTH (/ (image-width PADDLE) 2)) 300)))
