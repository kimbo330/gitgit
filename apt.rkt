#lang racket
(require gigls/unsafe)

;Problem 3
;;; Procedure:
;;;   dwelling
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Creates an image of an apartment based on the width and height as
;;;   defined by the user.
;;; Produces:
;;;   apt, an image
;;; Preconditions:
;;;   width > 0
;;;   height > 0
;;;   width <= (* 2 height)
;;;   height <= (* 2 width)
;;; Postconditions:
;;;   The output is an image. (image? (dwelling width height)) holds.
;;;   If height > width, there is a moon (a yellow circle) in the upper-left
;;;   corner of the image. If width >= height, there is no moon in the image.
;;;   If area (* width height) >= 40000, there are two trees.
;;;   If 20000 <= area (* width height) >= 40000, there is one tree.
;;;   If area (* width height) <= 20000, there are no trees.


(define dwelling
  (lambda (width height)
    (context-set-fgcolor! "black")
    (context-set-bgcolor! "midnightblue")
    (let ([apt (image-new width height)])
      (context-set-brush! "1. Pixel")
      (let* ([building-height (* height 0.6)]
             [building-width (* width 0.6)])
        (image-select-rectangle! apt REPLACE
                                 (* 0.2 width) (- height building-height)
                                 building-width building-height)
        (image-stroke-selection! apt)
        (context-set-fgcolor! "khaki")
        (image-fill-selection! apt)
        (context-set-fgcolor! "lightgoldenrodyellow")
        (when
            (> height width)
          (image-select-ellipse! apt REPLACE (* .1 width) (* .1 height)
                                 (* .2 width) (* .2 width)))
        (if
         (> height width)
         (image-fill-selection! apt)
         (image-select-nothing! apt))
        (cond
          [(and (>= 40000 (* width height)) (<= 20000 (* width height))
                (context-set-fgcolor! "olivedrab")
                (image-select-polygon! apt REPLACE
                                       (position-new (* .1 width) (* .85 height))
                                       (position-new (* .15 width) (* .8 height))
                                       (position-new (* .2 width) (* .85 height))
                                       (position-new (* .2 width) (* .9 height))
                                       (position-new (* .15 width) (* .95 height))
                                       (position-new (* .1 width) (* .9 height)))
                (image-select-rectangle! apt ADD
                                         (* .145 width) (* .9 height)
                                         (* .02 width) (* .1 height))
                (image-fill-selection! apt))]
          [(>= 20000 (* width height))
           (image-select-nothing! apt)]
          [(<= 40000 (* width height))
           (context-set-fgcolor! "olivedrab")
           (image-select-polygon! apt REPLACE
                                  (position-new (* .1 width) (* .85 height))
                                  (position-new (* .15 width) (* .8 height))
                                  (position-new (* .2 width) (* .85 height))
                                  (position-new (* .2 width) (* .9 height))
                                  (position-new (* .15 width) (* .95 height))
                                  (position-new (* .1 width) (* .9 height)))
           (image-select-rectangle! apt ADD
                                    (* .145 width) (* .9 height)
                                    (* .02 width) (* .1 height))
           (image-fill-selection! apt)
           (image-select-polygon! apt REPLACE
                                  (position-new (* .2 width) (* .85 height))
                                  (position-new (* .25 width) (* .8 height))
                                  (position-new (* .3 width) (* .85 height))
                                  (position-new (* .3 width) (* .9 height))
                                  (position-new (* .25 width) (* .95 height))
                                  (position-new (* .2 width) (* .9 height)))
           (image-select-rectangle! apt ADD
                                    (* .245 width) (* .9 height)
                                    (* .02 width) (* .1 height))
           (image-fill-selection! apt)])
        (image-select-rectangle! apt REPLACE
                                 (* .3 width) (- height (* .9 building-height))
                                 (* 2/3 building-width) (* 2/3 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .3 width) (- height (* 4/5 building-height))
                                 (* 2/3 building-width) (* .05 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .3 width) (- height (* 3/5 building-height))
                                 (* 2/3 building-width) (* .06 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .3 width) (- height (* 2/5 building-height))
                                 (* 2/3 building-width) (* .05 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .4 width) (- height (* .9 building-height))
                                 (* .01 width) (* 2/3 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .6 width) (- height (* .9 building-height))
                                 (* .01 width) (* 2/3 building-height))
        (image-select-rectangle! apt SUBTRACT
                                 (* .5 width) (- height (* .9 building-height))
                                 (* .01 width) (* 2/3 building-height))
        (image-stroke-selection! apt)
        (context-set-fgcolor! "lightsteelblue")
        (image-fill-selection! apt)
        (image-select-rectangle! apt REPLACE
                                 (* 1/2 width) (- height (* 0.2 building-height))
                                 (* 1/5 building-width) (* .2 height))
        (context-set-fgcolor! "thistle")
        (image-fill-selection! apt)
        (image-select-rectangle! apt REPLACE
                                 (* .18 width) (- height (* 1.05 building-height))
                                 (* 1.08 building-width) (* .05 building-height))
        (context-set-fgcolor! "peru")
        (image-fill-selection! apt)
        (context-set-fgcolor! "black")
        (image-stroke-selection! apt)
        (image-select-nothing! apt)
        apt))))

(image-show (dwelling 150 190))
(image-show (dwelling 500 700))
(image-show (dwelling 300 180))
(image-show (dwelling 120 120))



