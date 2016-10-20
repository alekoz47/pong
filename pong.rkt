;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;simple two-player pong

;;================
;;Constants:

(define WIDTH 1300)
(define HEIGHT 680)
(define BALL (circle 10 "solid" "white"))
(define PADDLE (rectangle (* 2 (image-width BALL)) 150 "solid" "white"))
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

(define-struct ball (pos vel=))
;;Ball is (make-ball Position Velocity)
#;
(define (fn-for-ball b)
  (... (fn-for-pos (ball-pos b))
       (fn-for-vel (ball-vel b))))

;;Paddle is one of:
;;- empty
;;- (cons pos Paddle)
#;
(define (fn-for-paddle p)
  (cond [(empty? p) (...)]
        [else (... (first p)
                   (fn-for-paddle (rest p)))]))

(define-struct world (ball paddle))
;;World is (make-world Ball Paddle)
#;
(define (fn-for-world w)
  (... (fn-for-ball (world-ball w))
       (fn-for-paddle (world-paddle w))))

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
                  (- (pos-y (first (world-paddle w)))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (first (world-paddle w)))
                     (/ (image-height PADDLE) 2))))
         (return w "left")]
        [(and (> (pos-x (ball-pos (world-ball w))) (/ WIDTH 2))
              (>= (pos-y (ball-pos (world-ball w)))
                  (- (pos-y (rest (world-paddle w)))
                     (/ (image-height PADDLE) 2)))
              (<= (pos-y (ball-pos (world-ball w)))
                  (+ (pos-y (rest (world-paddle w)))
                     (/ (image-height PADDLE) 2))))
         (return w "right")]
        [else (serve w)]))

;;World String -> World
;;builds new worldstate after ball bounce, inverts velocity
(define [return w s]
  (make-world
   (make-ball
    (ball-reset (world-ball w) s)
    (make-vel (- 0 (round-five (vel-x (ball-vel (world-ball w)))))
              (round-five (vel-y (ball-vel (world-ball w))))))
   (world-paddle w)))

;;Pos String -> Pos
;;resets ball position
(define [ball-reset wb s]
  (cond [(string=? s "left")
         (make-pos (- (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (+ (round-five (pos-y (ball-pos (world-ball w))))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb))))))]
        [(string=? s "right")
         (make-pos (+ (round-five (pos-x (ball-pos wb)))
                      (* BALL-SPEED (round-five (vel-x (ball-vel wb)))))
                   (- (round-five (pos-y (ball-pos (world-ball w))))
                      (* BALL-SPEED (round-five (vel-y (ball-vel wb))))))]))

;;World -> World
;;switches angle randomly
(define [serve w]
  (if (= (random 2) 1)
      (make-world (make-ball
                   (make-pos (/ WIDTH 2) (/ HEIGHT 2))
                   (make-vel (- 0 (/ 1 (sqrt 2))) (/ 1 (sqrt 2)))
                   BALL-SPEED)
                  (world-paddle w))
      (make-world (make-ball
                   (make-pos (/ WIDTH 2) (/ HEIGHT 2))
                   (make-vel (/ 1 (sqrt 2)) (/ 1 (sqrt 2)))
                   BALL-SPEED)
                  (world-paddle w))))

;;World -> World
;;bounces ball off wall
(define [bounce-ball w]
  (cond [(or (>= (round-five (pos-y (ball-pos (world-ball w))))
                 (- HEIGHT PADDING))
             (<= (round-five (pos-y (ball-pos (world-ball w))))
                 (+ 0 PADDING)))
         (make-world
          (make-ball
           (make-pos (+ (round-five (pos-x (ball-pos (world-ball w))))
                        (* BALL-SPEED
                           (round-five (vel-x (ball-vel (world-ball w))))))
                     (+ (round-five (pos-y (ball-pos (world-ball w))))
                        (* BALL-SPEED
                           (round-five (vel-y (ball-vel (world-ball w)))))))
           (make-vel (round-five (vel-x (ball-vel (world-ball w))))
                     (- 0 (round-five (vel-y (ball-vel (world-ball w))))))
           BALL-SPEED)
          (make-paddle
           (make-pos (pos-x (paddle-pos (world-paddle w)))
                     (pos-y (paddle-pos (world-paddle w))))
           (paddle-life (world-paddle w))
           (paddle-score (world-paddle w))))]
        [else (make-world
               (make-ball
                (make-pos (- (round-five (pos-x (ball-pos (world-ball w))))
                             (* BALL-SPEED
                                (round-five (vel-x (ball-vel (world-ball w))))))
                          (+ (round-five (pos-y (ball-pos (world-ball w))))
                             (* BALL-SPEED
                                (round-five (vel-y (ball-vel (world-ball w)))))))
                (make-vel (- 0 (round-five (vel-x (ball-vel (world-ball w)))))
                          (round-five (vel-y (ball-vel (world-ball w)))))
                BALL-SPEED)
               (make-paddle
                (make-pos (pos-x (paddle-pos (world-paddle w)))
                          (pos-y (paddle-pos (world-paddle w))))
                (paddle-life (world-paddle w))
                (paddle-score (world-paddle w))))]))

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
              (world-paddle w)))

;;Number -> Number
;;truncates inexact number to 5 decimal places
(define [round-five n]
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;World -> Image
;;render world on screen at positions
(define [render w]
  (cond [(= (paddle-life (world-paddle w)) 0)
         (overlay
          (text (number->string (paddle-score (world-paddle w)) 10) 36 "red")
          (place-images (list BALL PADDLE)
                        (list (make-posn
                               (pos-x (ball-pos (world-ball w)))
                               (pos-y (ball-pos (world-ball w))))
                              (make-posn
                               (pos-x (paddle-pos (world-paddle w)))
                               (pos-y (paddle-pos (world-paddle w)))))
                        MTS))]
        [else
         (place-images (list BALL PADDLE)
                       (list (make-posn
                              (pos-x (ball-pos (world-ball w)))
                              (pos-y (ball-pos (world-ball w))))
                             (make-posn
                              (pos-x (paddle-pos (world-paddle w)))
                              (pos-y (paddle-pos (world-paddle w)))))
                       MTS)]))

;;World KeyEvent -> World
;;w: paddle up 5, s: paddle down 5
(define [handle-key w ke]
  (cond [(key=? ke "w")
         (if (>= (- (pos-y (paddle-pos (world-paddle w)))
                    ( * 0.5 (image-height PADDLE))) 0)
             (make-world
              (make-ball
               (make-pos (pos-x (ball-pos (world-ball w)))
                         (pos-y (ball-pos (world-ball w))))
               (make-vel (vel-x (ball-vel (world-ball w)))
                         (vel-y (ball-vel (world-ball w))))
               BALL-SPEED)
              (make-paddle (make-pos
                            (pos-x (paddle-pos (world-paddle w)))
                            (- (pos-y (paddle-pos (world-paddle w)))
                               PADDLE-SPEED))
                           (paddle-life (world-paddle w))
                           (paddle-score (world-paddle w))))
             (make-world
              (make-ball
               (make-pos (pos-x (ball-pos (world-ball w)))
                         (pos-y (ball-pos (world-ball w))))
               (make-vel (vel-x (ball-vel (world-ball w)))
                         (vel-y (ball-vel (world-ball w))))
               BALL-SPEED)
              (make-paddle (make-pos
                            (pos-x (paddle-pos (world-paddle w)))
                            (pos-y (paddle-pos (world-paddle w))))
                           (paddle-life (world-paddle w))
                           (paddle-score (world-paddle w)))))]
        [(key=? ke "s")
         (if (<= (+ (pos-y (paddle-pos (world-paddle w)))
                    (* 0.5 (image-height PADDLE))) HEIGHT)
             (make-world
              (make-ball
               (make-pos (pos-x (ball-pos (world-ball w)))
                         (pos-y (ball-pos (world-ball w))))
               (make-vel (vel-x (ball-vel (world-ball w)))
                         (vel-y (ball-vel (world-ball w))))
               BALL-SPEED)
              (make-paddle (make-pos
                            (pos-x (paddle-pos (world-paddle w)))
                            (+ (pos-y (paddle-pos (world-paddle w)))
                               PADDLE-SPEED))
                           (paddle-life (world-paddle w))
                           (paddle-score (world-paddle w))))
             (make-world
              (make-ball
               (make-pos (pos-x (ball-pos (world-ball w)))
                         (pos-y (ball-pos (world-ball w))))
               (make-vel (vel-x (ball-vel (world-ball w)))
                         (vel-y (ball-vel (world-ball w))))
               BALL-SPEED)
              (make-paddle (make-pos
                            (pos-x (paddle-pos (world-paddle w)))
                            (pos-y (paddle-pos (world-paddle w))))
                           (paddle-life (world-paddle w))
                           (paddle-score (world-paddle w)))))]
        [else
         (make-world
          (make-ball
           (make-pos (pos-x (ball-pos (world-ball w)))
                     (pos-y (ball-pos (world-ball w))))
           (make-vel (vel-x (ball-vel (world-ball w)))
                     (vel-y (ball-vel (world-ball w))))
           BALL-SPEED)
          (make-paddle
           (make-pos (pos-x (paddle-pos (world-paddle w)))
                     (pos-y (paddle-pos (world-paddle w))))
           (paddle-life (world-paddle w))
           (paddle-score (world-paddle w))))]))

;;World -> Boolean
(define [continue? w]
  (cond [(= (paddle-life (world-paddle w)) 0) true]
        [else false]))

;;================
;;Run

(main
 (make-world
  (make-ball (make-pos (- WIDTH (image-height BALL)) (/ HEIGHT 2))
             (make-vel (- 0 (/ 1 (sqrt 2))) (- 0 (/ 1 (sqrt 2))))
             BALL-SPEED-DEFAULT)
  (make-paddle (make-pos (/ (image-width PADDLE) 2) 300)
               4 0)))
