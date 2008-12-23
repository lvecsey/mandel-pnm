#!/usr/bin/clisp -ansi -Kfull -E iso-8859-1
(load "mandelpnm.lisp")
(in-package "MANDELPNM")

;(defparameter +width+ 1280)
;(defparameter +height+ 1024)
(defparameter +width+ 720)
(defparameter +height+ 480)
;(defparameter +width+ 1280)
;(defparameter +height+ 720)

(defun movie-render-short ()
  (writerender-dev_stdout-simulation-expansions +width+ +height+ +keyframes+ (/ (frames-ntsc 26) 1000)))

(defun movie-render-full ()
  (writerender-dev_stdout-simulation-expansions +width+ +height+ +keyframes+ (frames-ntsc 26)))

(defun movie-render-end (width height keyframes)
  (let ((frames (/ (frames-ntsc 26) 10)))
    (writerender-dev_stdout-simulation-expanded-frames width height (nthcdr (floor (- frames 1000)) (keyframe-expansions keyframes frames)))))

;(movie-render-end 720 480 +keyframes+)
;(movie-render-short)
(movie-render-full)
