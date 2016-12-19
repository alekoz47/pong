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
(define BALL (circle 10 "solid" "white"))
(define PADDING (/ (image-width BALL) 2))
(define PADDLE (rectangle PADDING 75 "solid" "white"))
(define DIVIDER
  (local ((define (divider-acc img acc)
            (cond ((>= (image-height img) HEIGHT) img)
                  (else
                   (if (= (modulo acc 2) 0)
                       (divider-acc
                        (above (rectangle PADDING
                                          40 "solid" "white") img)
                        (add1 acc))
                       (divider-acc
                        (above (rectangle PADDING
                                          40 "solid" "black") img)
                        (add1 acc)))))))
    (divider-acc (rectangle PADDING 40 "solid" "black") 0)))
(define TICK-SPEED 0.015)
(define BALL-SPEED 5)
(define PADDLE-SPEED 10)
(define PADDLE-FACTOR 1.25)
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
              (move-paddle (world-paddle1 w))
              (move-paddle (world-paddle2 w))))

;;World -> Element
(define (choose-paddle w)
  (cond ((>= (/ WIDTH 2)
             (posn-x (element-pos (world-ball w))))
         (world-paddle1 w))
        (else
         (world-paddle2 w))))

;;Element Element -> Element
(define (move-ball b p)
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
         (advance-element b))))

;;Element Element -> Element
(define (left/right-ball b p)
  (cond ((and (>= (posn-y (element-pos b))
                  (- (posn-y (element-pos p))
                     (/ (image-height PADDLE) 2)))
              (<= (posn-y (element-pos b))
                  (+ (posn-y (element-pos p))
                     (/ (image-height PADDLE) 2))))
         (advance-element
          (make-element (element-pos b)
                        (make-posn (- 0 (posn-x (element-vel b)))
                                   (posn-y (element-vel b)))
                        (* PADDLE-FACTOR (element-speed b)))))
        (else
         (make-element (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                       (reset-vel p)
                       BALL-SPEED))))

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
(define (advance-element e)
  (make-element (make-posn (+ (posn-x (element-pos e))
                              (* (element-speed e)
                                 (posn-x (element-vel e))))
                           (- (posn-y (element-pos e))
                              (* (element-speed e)
                                 (posn-y (element-vel e)))))
                (element-vel e)
                (element-speed e)))

;;Element -> Element
(define (move-paddle p)
  (cond ((and (>= (- HEIGHT (/ (image-height PADDLE) 2))
                  (posn-y (element-pos p)))
              (<= (/ (image-height PADDLE) 2)
                  (posn-y (element-pos p))))
         (advance-element p))
        (else
         (advance-element
          (make-element (element-pos p)
                        (make-posn (posn-x (element-vel p))
                                   (- 0 (* 2 (posn-y (element-vel p)))))
                        (element-speed p))))))

;;World -> Image
(define (render w)
  (overlay DIVIDER
           (place-images (list BALL PADDLE PADDLE)
                         (list (element-pos (world-ball w))
                               (element-pos (world-paddle1 w))
                               (element-pos (world-paddle2 w)))
                         MTS)))

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
      (make-element (element-pos p)
                    (make-posn 0 1)
                    (element-speed p))
      (make-element (element-pos p)
                    (make-posn 0 -1)
                    (element-speed p))))

;;World KeyEvent -> World
(define (handle-release w ke)
  (cond ((or (key=? ke "w") (key=? ke "s"))
         (make-world (world-ball w)
                     (reset-paddle ke (world-paddle1 w))
                     (world-paddle2 w)))
        ((or (key=? ke "up") (key=? ke "down"))
         (make-world (world-ball w)
                     (world-paddle1 w)
                     (reset-paddle ke (world-paddle2 w))))
        (else w)))

;;KeyEvent Paddle -> Paddle
(define (reset-paddle ke p)
  (if (or (key=? ke "w") (key=? ke "up"))
      (make-element (element-pos p)
                    (make-posn 0 0)
                    (element-speed p))
      (make-element (element-pos p)
                    (make-posn 0 0)
                    (element-speed p))))

;;================================
;;Run:

(main
 (make-world
  (make-element (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                (make-posn 0.701 -0.701)
                BALL-SPEED)
  (make-element (make-posn (* (/ 3 2) PADDING)
                           (/ HEIGHT 2))
                (make-posn 0 0)
                PADDLE-SPEED)
  (make-element (make-posn (- WIDTH
                              (* (/ 3 2) PADDING))
                           (/ HEIGHT 2))
                (make-posn 0 0)
                PADDLE-SPEED)))
