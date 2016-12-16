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
(define BALL-SPEED 10)
(define PADDLE-SPEED 40)
(define MTS (empty-scene WIDTH HEIGHT "black"))

;;================================
;;Data definitions:

(define-struct element (pos vel speed))
;;Element is (make-element Posn Posn Integer)
;;Assume:
;;- Vel Posn -1 -> 1

(define-struct world (ball paddle1 paddle2))
;;World is (make-world Element Element Element Posn)
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

;;World -> World
(define (tock w)
  (make-world (move-ball (world-ball w)
                         (choose-paddle w))
              (world-paddle1 w)
              (world-paddle2 w)))

;;World -> Element
(define (choose-paddle w)
  (cond ((<= (/ WIDTH 2)
             (posn-x (element-pos (world-ball w))))
         (world-paddle1 w))
        (else
         (world-paddle2 w))))

;;Element Element -> Element
(define (move-ball b p)
  (cond ((or (<= (posn-x (element-pos b))
                 (+ PADDING (image-width PADDLE)))
             (>= (posn-x (element-pos b))
                 (- WIDTH (+ PADDING (image-width PADDLE)))))
         (left/right-ball b p))
        ((or (>= (posn-y (element-pos b))
                 (- HEIGHT PADDING))
             (<= (posn-y (element-pos b))
                 PADDING))
         (up/down-ball b))
        (else
         (advance-element b))))

;;Element Element -> Element
(define (left/right-ball b p)
  (cond ((and ((>= (posn-y (element-pos b))
                   (- (posn-y (element-pos p))
                      (/ (image-height PADDLE) 2))))
              (<= (posn-y (element-pos b))
                  (+ (posn-y (element-pos p))
                     (/ (image-height PADDLE) 2))))
         (advance-element
          (make-element (element-pos b)
                        (make-posn (- 0 (posn-x (element-vel b)))
                                   (posn-y (element-vel b)))
                        (element-speed b))))
        (else
         (make-element (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                       (reset-vel p)
                       10))))

;;Element -> Posn
(define (reset-vel p)
  (cond ((<= (/ WIDTH 2) (posn-x (element-pos p)))
         (make-posn 0.707 -0.707))
        (else
         (make-posn -0.707 0.707))))

;;Element -> Element
(define (up/down-ball b)
  (advance-element
   (make-element (element-pos b)
                 (make-posn (posn-x (element-vel b))
                            (- 0 (posn-y (element-vel b))))
                 (element-speed b))))

;;Element -> Element
(define (advance-element b)
  (make-element (make-posn (+ (posn-x (element-pos b))
                              (* (element-speed b)
                                 (posn-x (element-vel b)))))
                (make-posn (- (posn-y (element-pos b))
                              (* (element-speed b)
                                 (posn-y (element-vel b)))))
                (element-speed b)))

;;World -> Image
(define (render w)
  (place-images (list BALL PADDLE PADDLE)
                (list (element-pos (world-ball w))
                      (element-pos (world-paddle1 w))
                      (element-pos (world-paddle2 w)))
                MTS))

;;World KeyEvent -> World
(define (handle-key w ke)
  (cond ((or (key=? ke "w") (key=? ke "s"))
         (make-world (world-ball w)
                     (set-paddle ke (world-paddle1 w))
                     (world-paddle2 w)))
        ((or (key=? ke "up") (key=? ke "down"))
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (set-paddle ke (world-paddle2 w))))
        (else w)))

;;KeyEvent Paddle -> Paddle
(define (set-paddle ke p)
  (if (or (key=? ke "w") (key=? ke "up"))
      (make-paddle (element-pos p)
                   (make-posn 0 1)
                   (element-speed p))
      (make-paddle (element-pos p_)
                   (make-posn 0 -1)
                   (element-speed p))))

;;World KeyEvent -> World
;;stop moving paddle
#;
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

#;
(main
 (make-world
  (make-ball (make-pos (/ WIDTH 2) (/ HEIGHT 2))
             (make-vel (- 0 (/ 1 (sqrt 2))) (- 0 (/ 1 (sqrt 2)))))
  (make-paddle (make-pos (/ (image-width PADDLE) 2) (/ HEIGHT 2))
               (make-vel 0 0))
  (make-paddle (make-pos (- WIDTH (/ (image-width PADDLE) 2)) (/ HEIGHT 2))
               (make-vel 0 0))))
