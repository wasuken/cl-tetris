(defpackage cl-tetris
  (:use :cl :mylib)
  (:export :main))
(in-package :cl-tetris)

(defparameter *map* '())
(defparameter *width* 10)
(defparameter *height* 20)
(defparameter *block* 0)
(defparameter *blank* 1)
(defparameter *moving-blocks* 2)

;;; 初期mapの生成
(defun gen-map (&optional (node *blank*))
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) node)
								(mylib:range 1 *width*)))
		  (mylib:range 1 *height*)))

(defun gen-pie (point-lst &optional (block *block*) (blank *blank*))
  (loop for x from 0 to 3
	 collect (loop for y from 0 to 3
				collect (cond ((find `(,x . ,y) point-lst :test #'equal)
							   block)
							  (t blank)))))

(defun replace-map (map x y v)
  (loop for i from 0 to (1- (length map))
	 collect (loop for j from 0 to (1- (length (car map)))
				collect (cond ((and (= x i) (= y j))
							   v)
							  (t (nth j (nth i map)))))))

(defun pie-convert (block before after)
  (mapcar #'(lambda (x)
			  (mapcar #'(lambda (y)
						  (if (= before y)
							  after
							  y))
					  x))
		  block))

;;; utils
(defun drop-while (f lst)
  (cond ((funcall f (car lst))
		 (drop-while f (cdr lst)))
		(t lst)))

(defun map-print (map)
  (format t "============================~%")
  (format t "~{~A~%~}" (mapcar #'(lambda (x) (format nil "~{~A~}" x)) map))
  (format t "============================~%"))

;; blocks
;;; おっぱい
(defparameter *o-pie*
  (gen-pie '((1 . 1) (1 . 2) (2 . 1) (2 . 2))))

;;; i-pie
(defparameter *i-pie*
  (gen-pie '((0 . 1) (1 . 1) (2 . 1) (3 . 1))))

;;; s-pie
(defparameter *s-pie*
  (gen-pie '((1 . 1) (2 . 1) (2 . 2) (3 . 2))))

;;; z-pie
(defparameter *z-pie*
  (gen-pie '((1 . 2) (2 . 1) (2 . 2) (3 . 1))))

;;; j-pie
(defparameter *j-pie*
  (gen-pie '((1 . 1) (1 . 2) (2 . 2) (3 . 2))))

;;; l-pie
(defparameter *l-pie*
  (gen-pie '((1 . 1) (1 . 2) (2 . 1) (3 . 1))))

(defparameter *t-pie*
  (gen-pie '((1 . 1) (2 . 1) (2 . 2) (3 . 1))))

;; map operations
;;; put
;;; TODO: blockの上がblankである場合は切り詰める処理をいれる。
(defun put-block (map block &optional (point-x 3))
  (let* ((new-map map)
		 (manuf-block (drop-while #'(lambda (x)
									  (every #'(lambda (y) (= *blank* y))
											 x))
								  block))
		 (manuf-block-y (length manuf-block))
		 (manuf-block-x (length (car manuf-block))))
	;; 初期位置でおきかえる。
	(loop for i from 0 to (1- manuf-block-y)
	   do (loop for j from point-x to (+ point-x manuf-block-x -1)
			 do (setf new-map
					  (replace-map new-map i j (nth (- j point-x) (nth i manuf-block))))))
	new-map))

(defun check-apl (map node-lst apl)
  (let ((max-y (length map))
		(fst (car apl)))
	(cond ((null apl) t)
		  ((>= (car fst) max-y) nil)
		  (t (let* ((eq (find (nth (cdr fst)
								   (nth (car fst) map))
							  node-lst)))
			   ;; (print "********************")
			   ;; (print apl)
			   ;; (format t "x: ~A, y: ~A~%" (car fst) (cdr fst))
			   ;; (print (nth (car fst) (nth (cdr fst) map)))
			   ;; (print node-lst)
			   ;; (print "********************")
			   (cond (eq (check-apl map node-lst (cdr apl)))
					 (t nil))))))
  )

(defun update-map (map apl value)
  (if (null apl)
	  map
	  (let ((point (car apl))
			   (updating-map map))
		   (setf (nth (cdr point) (nth (car point) updating-map)) value)
		   (update-map updating-map (cdr apl) value))))

;;; move
;;; 全体を動かすかんじで
(defun move (map &optional (active-point-lst nil))
  (when (null active-point-lst)
	(loop for x from 0 to (1- (length map))
	   do (loop for y from 0 to (1- (length (car map)))
			 do (if (= (nth y (nth x map)) *moving-blocks*)
					(setf active-point-lst
						  (append active-point-lst `((,x . ,y))))))))
  ;; check
  ;; 現状はゆーざの操作関係なく勝手に落ちてく状態
  (let ((moved-apl (mapcar #'(lambda (x) `(,(1+ (car x)) . ,(cdr x)))
						   active-point-lst)))
	;; (print "###############################")
	;; (print (check-apl map `(,*blank* ,*moving-blocks*) moved-apl))
	;; (print active-point-lst)
	;; (print "###############################")
	(cond ((check-apl map `(,*blank* ,*moving-blocks*) moved-apl)
		   ;; move ok
		   (let ((moved-map (update-map (update-map map active-point-lst *blank*)
										moved-apl
										*moving-blocks*)))
			 (move moved-map moved-apl)))
		  ;; move failed
		  (t (update-map map active-point-lst *block*))))
  )

;;; delete

;;; debug

(defun var-print ()
  (format t "o~%")
  (map-print *o-pie*)
  (format t "i~%")
  (map-print *i-pie*)
  (format t "s~%")
  (map-print *s-pie*)
  (format t "z~%")
  (map-print *z-pie*)
  (format t "j~%")
  (map-print *j-pie*)
  (format t "l~%")
  (map-print *l-pie*)
  (format t "t~%")
  (map-print *t-pie*)
  (format t "map~%")
  (setf *map* (gen-map))
  (setf *map* (put-block *map* (pie-convert *t-pie* *block* *moving-blocks*)))
  (map-print *map*)
  (map-print (move *map*)))

;;; main
(defun main ()
  (var-print))
