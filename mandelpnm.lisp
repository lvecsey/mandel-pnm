(defpackage :mandelpnm (:use :cl))

(in-package :mandelpnm)

; mandelpnm will stream a set of .pnm images to stdout. You can pipe this
; to an mpeg4 image encoder, or to encodedv first and then another encoder.

; You can also render a single .pnm image directly to a file. Usually this
; is done with the 'nomem' code path for very large desktop backgrounds.

; CLISP
; (setf (long-float-digits) 128) 
; (long-float-digits)

; These image frame description resemble the XFRACTINT info <tab> page.

(defstruct frame
   (topleft-x -2.5L0 :type long-float)
   (topleft-y 1.5L0 :type long-float)
   (bottomright-x 1.5L0 :type long-float)
   (bottomright-y -1.5L0 :type long-float)
   (center-x -0.5L0 :type long-float)
   (center-y 0.0L0 :type long-float)
   (mag 0.666666667L0 :type long-float)
   (x-mag-factor 1.0L0 :type long-float)
   (rotation 0.0L0 :type long-float)
   (skew 0.0L0 :type long-float)
   (iteration-max 150 :type integer))

(defun make-image-data (w h)
  (make-array (list (* w h 3)) :initial-element 0 :element-type '(unsigned-byte 8)))

(defun make-image(w h)
  (list w h (make-image-data w h)))

(defun flatten-coordinate(x y)
  (+ x (* y (* x 3))))

(defun make-displaced-array(flat-array x y)
  (make-array 3 :element-type '(unsigned-byte 8) :displaced-to flat-array :displaced-index-offset (flatten-coordinate x y))) 

(defun image-width (image)
  (first image))

(defun image-height (image)
  (second image))

(defun image-data (image)
  (third image))

; Some of the next few functions including the core inner loop are based on
; http://uint32t.blogspot.com/2008/04/mandelbrot-generator-in-common-lisp.html

(defun palette-map (iterations-taken max-iterations)
  (let (depth)
    (if (= iterations-taken max-iterations)
	(setf depth 0)
	(progn
	  (setf depth (truncate (* 255 (/ iterations-taken 1000))))))
    (list (min 180 (* 1 depth))
	  (min 180 (* 5 depth))
	  (min 180 (* 20 depth)))))

(defun set-image-color (stride data x y iterations-taken max-iterations)
  (let ((rgb-colors (palette-map iterations-taken max-iterations)))
    (setf (aref data (+ (* 3 x) (* stride y))) (first rgb-colors))
    (setf (aref data (+ 1 (* 3 x) (* stride y))) (second rgb-colors))
    (setf (aref data (+ 2 (* 3 x) (* stride y))) (third rgb-colors))))

(defun set-image-color-write (file rgb-triplet iterations-taken max-iterations)
  (let ((rgb-colors (palette-map iterations-taken max-iterations)))
    (setf (aref rgb-triplet 0) (first rgb-colors))
    (setf (aref rgb-triplet 1) (second rgb-colors))
    (setf (aref rgb-triplet 2) (third rgb-colors))
    (ext:write-byte-sequence rgb-triplet file :start 0 :end 3 :no-hang NIL :interactive NIL)))

(defun gen-setcolor-image-func (stride data)
  (lambda (x y iterations-taken max-iterations)
    (set-image-color stride data x y iterations-taken max-iterations)))

(defun gen-setcolor-directwrite-func (file)
  (let ((rgb-triplet (make-array '3 :initial-element 0 :element-type '(unsigned-byte 8))))
    (lambda (x y iterations-taken max-iterations)
      (declare (ignore x y))
      (set-image-color-write file rgb-triplet iterations-taken max-iterations))))

(defun within-radius (z)
  (let ((r (realpart z))
        (i (imagpart z)))
    (<= (+ (* r r) (* i i))
        4)))

(defun calculate-mandelbrot-image (width height &key (max-iterations 1000)
                                   (real '(-2.5L0 1.5L0))
                                   (imag '(1.5L0 -1.5L0))
				   (pixel-result-func NIL)
				   (compute-layout :mem-work))
  (let* ((low-real (first real))
         (high-real (second real))
         (low-imag (first imag))
         (high-imag (second imag))
         (real-length (- high-real low-real))
         (imag-length (- high-imag low-imag))
         (real-by (/ real-length width))
         (imag-by (/ imag-length height)))
    (flet  ((do-it (start-x end-x start-cr)
	      (loop 
		 for y below height
		 for ci from low-imag by imag-by
		 do
		   (loop
		      for x from start-x to end-x
		      for cr from start-cr by real-by
		      do
                        (let* ((c (complex cr ci))
                               (iterations-taken
                                (loop
                                   for z = c then (+ (* z z) c)
                                   for iteration from 0 below max-iterations
                                   while (within-radius z)
                                   count iteration)))
                          (cond ((not (null pixel-result-func)) (funcall pixel-result-func (truncate x) (truncate y) iterations-taken max-iterations))))))))
      (defun mem-work ()
	(let* ((end-x (truncate (/ width 2))))
	  (progn 
	    (do-it 0 end-x low-real)
	    (do-it (1+ end-x) (1- width) (+ (* end-x real-by) low-real)))))
      (defun linear-work()
	(do-it 0 (1- width) low-real))
      (cond ((eq compute-layout :mem-work) (mem-work))
	    (t (linear-work))))))
	
(defparameter +PC_width+ 640)
(defparameter +PC_height+ 480)
(defparameter +SD480p_width+ 720)
(defparameter +SD480p_height+ 480)
(defparameter +HD720p_width+ 1280)
(defparameter +HD720p_height+ 720)

(defun render-frame(width height step pixel-result-func &key (compute-layout :mem-work))
  (with-slots ((topleft-x topleft-x) (topleft-y topleft-y) (bottomright-x bottomright-x) (bottomright-y bottomright-y) (iteration-max iteration-max)) step
    (calculate-mandelbrot-image width height :real (list topleft-x bottomright-x) :imag (list topleft-y bottomright-y) :max-iterations iteration-max :pixel-result-func pixel-result-func :compute-layout compute-layout)))

(defun spit-header(width height stream)
  (let ((header (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s header) 
      (format s "P6~%#generated by mandelpnm~%~a ~a~%255~%" width height)
      (let ((header-bytes (ext:convert-string-to-bytes header CHARSET:UTF-8)))
	(ext:write-byte-sequence header-bytes stream :start 0 :end (length header) :no-hang NIL :interactive NIL)))))

(defun spit-data(image stream)
  (let ((width (first image)) (height (second image)))
    (ext:write-byte-sequence (third image) stream :start 0 :end (* width height 3) :no-hang NIL :interactive NIL)))

(defparameter +keyframes+ (list 

(make-frame :topleft-x -2.5L0 :topleft-y 1.5L0 :bottomright-x 1.5L0 :bottomright-y -1.5L0
:center-x -0.5L0 :center-y -1.5L0 :mag 6.66666667L0
:x-mag-factor 1.0L0 :iteration-max 150)

(make-frame :topleft-x -0.6784037558685445L0    :topleft-y -0.4665970772442588L0 
:bottomright-x -0.1212832550860719L0 :bottomright-y -0.8844374528311133L0
:center-x -0.3998435054773082L0 :center-y -0.6755172650376861L0 :mag 4.78651685L0
:x-mag-factor 1.0L0 :iteration-max 750)

(make-frame :topleft-x -0.7429072735545574L0       :topleft-y -0.2743642010254950L0
:bottomright-x -0.6568507047634542L0    :bottomright-y -0.3389066276188224L0
:center-x -0.6998789891590058L0   :center-y -0.3066354143221587L0 :mag 30.9873692L0
:x-mag-factor 1.0L0 :iteration-max 1000)

(make-frame :topleft-x -0.7167696736957242L0          :topleft-y -0.2980598065419851L0
:bottomright-x -0.7167497132590027L0       :bottomright-y -0.2980747768695262L0
:center-x -0.7167596934773635L0     :center-y -0.2980672917057556L0   :mag 133597.611L0
:x-mag-factor 1.0L0 :iteration-max 2000)

))

(defun coordset-from-frames(keyframes)
  (loop for step in keyframes
     for i from 0 upto (length keyframes)
	 collecting (with-slots ((x topleft-x) (y topleft-y)) step
		      (list (coerce x 'float) (coerce y 'float)))))

(defun zeroensure/ (quot divisor)
  (cond ((or (> quot 0) (< quot 0)) (/ quot divisor))
	(t 0)))

(defun interpolate-value (start-value end-value divisions count)
  (cond ((eq count 0) start-value)
	((eq count (- divisions 1)) end-value)
	((or (< count 0) (>= count divisions)) (error "count must be in the range 0 <= count < divisions"))
	(T (+ start-value (* (- end-value start-value) (zeroensure/ count (coerce divisions 'long-float)))))))

(defun interpolate-value2 (start-value end-value divisions count)
  (cond ((eq count 0) start-value)
	((eq count (- divisions 1)) end-value)
	((or (< count 0) (>= count divisions)) (error "count must be in the range 0 <= count < divisions"))
	(T (+ start-value (zeroensure/ (- (* end-value count) (* start-value count)) (coerce divisions 'long-float))))))

(defun interpolate-integervalue2 (start-value end-value divisions count)
  (cond ((eq count 0) start-value)
	((eq count (- divisions 1)) end-value)
	((or (< count 0) (>= count divisions)) (error "count must be in the range 0 <= count < divisions"))
	(T (floor (+ start-value (zeroensure/ (- (* end-value count) (* start-value count)) divisions))))))

(defun interpolated-frame (start-frame end-frame divisions count interpolate-func)
  (with-slots ((stopleft-x topleft-x) (stopleft-y topleft-y) (sbottomright-x bottomright-x) (sbottomright-y bottomright-y)
	       (scenter-x center-x) (scenter-y center-y) (smag mag) (sx-mag-factor x-mag-factor) (srotation rotation)
	       (sskew skew) (siteration-max iteration-max)) start-frame
    (with-slots ((etopleft-x topleft-x) (etopleft-y topleft-y) (ebottomright-x bottomright-x) (ebottomright-y bottomright-y)
		 (ecenter-x center-x) (ecenter-y center-y) (emag mag) (ex-mag-factor x-mag-factor) (erotation rotation)
		 (eskew skew) (eiteration-max iteration-max)) end-frame
      (make-frame :topleft-x (funcall interpolate-func stopleft-x etopleft-x divisions count)
		  :topleft-y (funcall interpolate-func stopleft-y etopleft-y divisions count)
		  :bottomright-x (funcall interpolate-func sbottomright-x ebottomright-x divisions count)
		  :bottomright-y (funcall interpolate-func sbottomright-y ebottomright-y divisions count)
		  :center-x (funcall interpolate-func scenter-x ecenter-x divisions count)
		  :center-y (funcall interpolate-func scenter-y ecenter-y divisions count)
		  :mag (funcall interpolate-func smag emag divisions count)
		  :x-mag-factor (funcall interpolate-func sx-mag-factor ex-mag-factor divisions count)
		  :rotation (funcall interpolate-func srotation erotation divisions count)
		  :skew (funcall interpolate-func sskew eskew divisions count)
		  :iteration-max (funcall #'interpolate-integervalue2 siteration-max eiteration-max divisions count)
		  ))))

; keyframes are a raw set of frame info into the mandelbrot
; mapped-keyframes is a list of tuples consisting of a frame number and an associated keyframe into the mandelbrot.

(defun write-image-core(image stream)
  (progn 
    (spit-header (image-width image) (image-height image) stream)
    (spit-data image stream)))

(defun write-image-file(image)
  (with-open-file (file "image0.pnm" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-image-core image file)))

(defun write-image-stdout(image)
  (write-image-core image *STANDARD-OUTPUT*))

(defun write-image-dev_stdout(image)
  (with-open-file (file "/dev/stdout" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-image-core image file)))

(defun keyframe-mapping (keyframes frames)
  "Return the frame numbers that contain keyframes"
  (let ((keyframe-location 0) (num-middle-keyframes (- (length keyframes) 2)))
    (let ((keyframe-spacing (/ frames (length keyframes))) (result-list NIL))
      (cond ((null keyframes) NIL)
	    ((eq (length keyframes) 2) (list (list 0 (first keyframes)) (list (- frames 1) (second keyframes))))
	    (t (progn
		 (setq result-list (list (cons 0 (first keyframes))))
		 (loop for step in (cdr keyframes)
		    for i from 0 below num-middle-keyframes
		    do (push (cons (floor (setq keyframe-location (+ keyframe-location keyframe-spacing))) step) result-list))
		 (push (cons (- frames 1) (car (last keyframes))) result-list)
		 (nreverse result-list)))))))

(defun keyframe-expansions (keyframes frames)
  (let ((mapped-keyframes (keyframe-mapping keyframes frames)))
    (let ((keyinfo (first mapped-keyframes)) (keyinfo-next (first (cdr mapped-keyframes))))
      (loop for x from 0 below frames
	 collecting (progn
	      (cond ((>= x (car keyinfo-next)) (progn
						 (setq mapped-keyframes (cdr mapped-keyframes))
						 (setq keyinfo (first mapped-keyframes))
						 (setq keyinfo-next (first (cdr mapped-keyframes))))))
	      (cond ((not (eq keyinfo-next NIL))
		     (interpolated-frame (cdr keyinfo) (cdr keyinfo-next) (- (car keyinfo-next) (car keyinfo)) (- x (car keyinfo)) #'interpolate-value))))))))

(defun writerender-dev_stdout-simulation-expanded-frames(width height frame-steps)
  (with-open-file (file "/dev/stdout" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (loop for step in frame-steps
	 do (let ((image (make-image width height)))
	      (let ((pixel-result-func (gen-setcolor-image-func (* 3 width) (image-data image))))
		(progn 
		  (render-frame width height step pixel-result-func)
		  (write-image-core image file)))))))

(defun writerender-dev_stdout-simulation-expansions(x y keyframes frames)
  (writerender-dev_stdout-simulation-expanded-frames x y (keyframe-expansions keyframes frames)))

(defun frames-ntsc(minutes)
  (* 29.97 60 minutes))

(defun combined-file(width height step)
  (let ((image (make-image width height)))
    (let ((pixel-result-func (gen-setcolor-image-func (* 3 width) (image-data image))))
      (progn
	(render-frame width height step pixel-result-func)
	(write-image-file image)))))

(defun combined-file-nomem(width height step)
  (with-open-file (file "image0.pnm" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (let ((pixel-result-func (gen-setcolor-directwrite-func file)))
      (progn
	(spit-header width height file)
	(render-frame width height step pixel-result-func :compute-layout :linear-work)))))

(defun check()
  (format t "mandelpnm: PASS OK~%"))

(defun work()
  (keyframe-mapping +keyframes+ (frames-ntsc 26)))
