(defpackage cl-tetris
  (:use :cl :mylib)
  (:export :main))
(in-package :cl-tetris)

(defparameter *map* '())
(defparameter *width* 10)
(defparameter *height* 20)
(defparameter *block* "#")
(defparameter *blank* "*")

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


;;; utils
(defun drop-while (f lst)
  (cond ((funcall f (car lst))
		 (drop-while f (cdr lst)))
		(t lst)))

(defun map-print (map)
  (format t "~{~A~%~}" (mapcar #'(lambda (x) (format nil "~{~A~}" x)) map)))

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
									  (every #'(lambda (y) (string= *blank* y))
											 x)
									  )
								  block))
		 (manuf-block-y (length manuf-block))
		 (manuf-block-x (length (car manuf-block))))
	;; 初期位置でおきかえる。
	(loop for i from 0 to (1- manuf-block-y)
	   do (loop for j from point-x to (+ point-x manuf-block-x -1)
			 do (setf new-map
					  (replace-map new-map i j (nth (- j point-x) (nth i manuf-block))))))
	new-map))

;;; move

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
  (setf *map* (put-block *map* *t-pie*))
  (map-print *map*))

;;; main
(defun main ()
  (var-print))
