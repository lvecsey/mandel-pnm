#!/usr/bin/clisp -ansi -Kfull -E iso-8859-1
(load "mandelpnm.lisp")
(in-package "MANDELPNM")

(defparameter +width+ 3360)
(defparameter +height+ 2074)

;(defparameter +width+ 336)
;(defparameter +height+ 207)

; generates an image0.pnm desktop background file.

(combined-file-nomem +width+ +height+ (third +keyframes+))
