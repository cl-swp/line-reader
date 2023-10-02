;;;-*- Mode: common-lisp; syntax: common-lisp; package: line-reader; base: 10 -*-
;;;
;;;; Line Based Reader Module
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014-2020, 2023 Seiji Koide <koide@ontolonomy.co.jp>
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this 
;;; software and associated documentation files (the "Software"), to deal in the Software 
;;; without restriction, including without limitation the rights to use, copy, modify, 
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to 
;;; permit persons to whom the Software is furnished to do so, subject to the following 
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies 
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE 
;;; USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;; History
;; -------
;; 2023/09/03  confirm on acl11.0b
;; 2023/02/10  added sbcl part using by sb-simple-streams
;; 2020/11/30  added new part for sbcl, which has a bug of latin-1 default external 
;;             format in case of user-defined stream.
;; 2014/05/06  File created.
;;; ==================================================================================

(cl:provide :line-reader)
#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-simple-streams))

(cl:defpackage :line-reader
  (:nicknames :line)
  (:use :common-lisp)
  (:shadow #:stream #:with-open-file #:read-line)
  (:export #:stream #:with-open-file #:read-line #:line-count #:line-position #:line-position-memo
           #:expose-one-line-buf #:expose-three-lines-buf 
           #:white-char-p #:skipbl #:next-token #:with-buffered-open-file
           #:simple-stream-read-line #:read-string
           #:peeknext-char #:getnext-char #:putback-char
           #:null-line-p #:update-line-count 
   ))

(in-package :line-reader)

(defvar *this-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))
;(defvar *that-file* (load-time-value (or *compile-file-pathname* *load-pathname*)))
;(defvar *what-file* (or #.*compile-file-pathname* *load-pathname*))

(defparameter *line-reader-home*
  (make-pathname :name nil :type nil :defaults *this-file*))
#|
(in-package :line)
(with-open-file (stream (make-pathname :name "linetest" :type "txt" :defaults *line-reader-home*))
  (initialize-line-count stream)
  (loop for line = (read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A ~A~%" (line-count stream) (line-position stream) line (expose-one-line-buf stream))
        (force-output t))
  (line-position-memo stream))

|#
#|
#-:MACOS :cd /home/seiji/ontologies/basics/
#+:MACOS :cd /Users/seiji/ontologies/basics/
(line:with-open-file (stream "22-rdf-syntax-ns.ttl" :external-format #+:allegro :utf-8 #+:sbcl :default)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))

:cd /home/seiji/allegro-projects/drawio
(line:with-open-file (stream "test10.drawio" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))

:cd /usr/share/mecab/dic/juman
(line:with-open-file (stream "juman.allsorted.csv" :external-format #+:allegro :utf-8 #+sbcl :default)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))
      
:cd \\LS220D95A\ontologies\BFO
(line:with-open-file (stream "bfo.owl" :external-format #+:allegro :utf-8 #+sbcl :default)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A~%" line-count line-position line)))

:cd \\LS220D95A\ontologies\Freebase
(line:with-open-file (stream "freebase-rdf-latest" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))
|#
;;;
;;;; Line Counting Stream 
;;;
;;; N-Triples are line-based data format. Namely, one triple per one line. 
;;; Even though Turtles and JSON-LD are not line-based, but it is important 
;;; to notice errors with line numbers in order for users to fix them.
;;;
;;; In the original SWCLOS, peeking one and more characters has been realized 
;;; in order to parse RDF/XML format data instead of making a state-transition network. 
;;; However, characters to be read actually exist in streams at hand, if the streams 
;;; come from files. The fact is that peeking characters is needless in the case, if it 
;;; is possible to go back to any file position which you want.
;;;
;;; This module provides line count numbers and file positions for every line to users. 
;;; This module will be base of new parsers for N-Triples, Turtles, JSON-LD, and RDF/XML. 
;;;

;;;
;;; <stream> is a stream that counts and provides line numbers of files. 
;;;

(defclass stream (#+:allegro excl::file-simple-stream #+:sbcl sb-simple-streams:file-simple-stream)
  ((line-count :initform 0 :accessor line-count
               :documentation "placeholder of line count, line number starts at 0.")
   (line-position :initform 0 :accessor line-position
                  :documentation "placeholder of current line position.")
   (previous-line-position :initform 0 :accessor previous-line-position
                           :documentation "placeholder of previous line position.")
   (line-position-memo :initform (make-array '(30) :fill-pointer 0 :adjustable t) :accessor line-position-memo
                       :documentation "line position memory.")
   )
  (:documentation "This stream has a sequence of line positions in the file.")
  )

(defun initialize-line-count (stream)
  (setf (previous-line-position stream) 0)
  (setf (line-position stream) 0)
  (setf (line-count stream) 0)
  (vector-push-extend (line-position stream) (line-position-memo stream)))

(declaim (inline update-line-count)) ; this is specically for sbcl
(defun update-line-count (stream)
  "when a line is read, this function should be called imediately.
   before an line read, line-count 0, file-position 0,
   after the first new line, line-count 1 and current position is set."
  (setf (previous-line-position stream) (line-position stream))
  (setf (line-position stream) (file-position stream))
  (prog1 (incf (line-count stream))
    (vector-push-extend (line-position stream) (line-position-memo stream)))
  )

(defun expose-one-line-buf (stream)
  "exposes one line of the current file position."
  (let ((this-pos (file-position stream)))
    (file-position stream (line-position stream))
    (multiple-value-prog1 (cl:read-line stream nil nil)
      (file-position stream this-pos))))

(defun expose-three-lines-buf (stream)
  "exposes three lines around the current file position."
  (let ((this-pos (file-position stream)))
    (file-position stream
                   (previous-line-position stream))
    (multiple-value-prog1
        (cons (cl:read-line stream)
              (cons (cl:read-line stream)
                    (list (cl:read-line stream nil nil))))
      (file-position stream this-pos))))

(defun read-line (stream &optional (eof-error-p t) eof-value recursive-p)
  (multiple-value-bind (line missing-newline-p) (cl:read-line stream eof-error-p eof-value recursive-p)
    (unless missing-newline-p (update-line-count stream))
    (when (not (null line))
      (setq line (string-right-trim '(#\Return #\Linefeed) line)))
    (values line missing-newline-p)))

#|
Above code is effective, too, in SBCL, but optional external-format :utf-8 causes an error.
No external-format sets default value ISO-8859-1 (8 bit code for Latin-1).
To escape this error, the following codes are developed.
|#
#+never
(let ((line-position-table (make-hash-table))
      (line-count 0)
      (line-position 0))
  (setf (gethash 0 line-position-table) 0)
  (defun line-count (stream)
    (declare (ignore stream))
    line-count)
  (defun (setf line-count) (newval stream)
    (declare (ignore stream))
    (setq line-count newval))
  (defun line-position (stream)
    (declare (ignore stream))
    line-position)
  (defun (setf line-position) (newval stream)
    (declare (ignore stream))
    (setq line-position newval))
  (defun update-line-count (stream)
    "Note that this function should be called after reading a NEWLINE"
    (setf (gethash line-count line-position-table) line-position)
    (setq line-position (file-position stream))
    (incf line-count))
  (defun expose-one-line-buf (stream)
    "exposes one line of the current file position."
    (let ((this-pos (file-position stream)))
      (file-position stream line-position)
      (prog1 (cl:read-line stream)
        (file-position stream this-pos))))
  (defun expose-three-lines-buf (stream)
    "exposes three lines around the current file position."
    (let ((this-pos (file-position stream)))
      (file-position stream (gethash (1- line-count) line-position-table)) ; previous line position
      (prog1
          (cons (cl:read-line stream)
                (cons (cl:read-line stream)
                      (list (cl:read-line stream nil nil))))
        (file-position stream this-pos))))
  (defun read-line (stream &optional (eof-error-p t) eof-value recursive-p)
    (prog1 (string-right-trim '(#\Return #\Linefeed)
                              (cl:read-line stream eof-error-p eof-value recursive-p))
      (update-line-count stream)))
  (defmacro with-open-file (varargs &rest body)
    `(cl:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs)) ; :utf-8 as default
       ,@body))
  )

#+allegro
(defun simple-stream-read-line (stream &optional (eof-error-p t) eof-value line)
  (multiple-value-bind (result stopped end)
      (excl:simple-stream-read-line stream eof-error-p eof-value line)
    (update-line-count stream)
    (when (and stopped (eq result eof-value)) 
      (return-from simple-stream-read-line (values eof-value 0)))
    (values result (or end (length result)))))

(defmacro with-open-file (varargs &rest body)
  "calling sequence: `with-open-file (stream filespec {options}*) {declaration}* {form}*'
   this macro adds class option for <line:stream>."
  `(cl:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs)
                      ':class 'stream)
     ,@body))

;;;
;;; Token Reader
;;;
;;; A token is delimited white characters, that is, spaces and tabs in N-Triples,
;;; furthermore, newlines and returns in Turtles and RDF/XML.
;;; As well, those characters satisfy predicate <delimiter-p>.

(defmacro white-char-p (char)
  "Space, Tab, Newline, Return, or Page ?"
  `(or (char= ,char #\Space)
       (char= ,char #\Tab)
       (char= ,char #\Newline)
       (char= ,char #\Return)
       (char= ,char #\Page)))

(defun skipbl (stream)
  "In this version, this function must be, excepting comment, used to eat up every newline."
  (declare (optimize (speed 3) (safety 0)))
  (let ((char (read-char stream nil :EOF)))
    (cond ((eq char :EOF) :EOF)            ; eof then exit
          ((char= char #\Newline)          ; newline
           (update-line-count stream)
           (skipbl stream))                ; loop by tail recursive
          ((or (char= char #\Space)
               (char= char #\Tab)
               (char= char #\Return)
               (char= char #\Page))
           (skipbl stream))                ; loop by tail recursive
          (t (unread-char char stream))))) ; else unread and exit

(defun next-token (stream)
  "reads and returns a next token. A token is a string delimited by one or more white characters,
   and one or more semicolons and commas, and closing angle bracket and colon for XML.
   Note that white chars and semicolons and commas. succeeding a token is not eaten up."
;;;  (declare (optimize (speed 3) (safety 0)))
  (skipbl stream)
  (coerce
   (loop for char character = (cl:read-char stream nil nil)
       until (or (eq char nil)   ; EOF
                 (when (or (char= char #\Space)
                           (char= char #\Tab)
                           (char= char #\Newline)
                           (char= char #\Return)
                           (char= char #\Page)
                           (char= char #\;)
                           (char= char #\,)
                           (char= char #\>)
                           (char= char #\:)
                           (char= char #\=))
                   (unread-char char stream)
                   t))
       collect char)
   'string))
#+allegro
(defun read-string (stream)
  (line:skipbl stream)
  (assert (char= #\" (read-char stream nil nil)))
  (excl::read-string stream #\"))
#|
#+sbcl
(defun read-string (stream)
  (line:skipbl stream)
  (assert (char= #\" (read-char stream nil nil)))
  (%read-string stream #\"))
#+sbcl
(defun %read-string (stream closech)
  (let ((stringbuf nil))
    (loop for ch = (cl:read-char stream nil nil)
         while ch
        do (cond ((char= ch closech)
                  (return-from %read-string (coerce (nreverse stringbuf) 'cl:string)))
                 ((char= ch #\\)
                  (push ch stringbuf)
                  (push (read-char stream) stringbuf))
                 (t (push ch stringbuf))))))
     
;;;(defun read-string (stream)
;;;  (unless (boundp sb-impl::*read-buffer*) (setq sb-impl::*read-buffer* nil))
;;;  (line:skipbl stream)
;;;  (assert (char= #\" (read-char stream nil nil)))
;;;  (sb-impl::read-string stream #\"))
|#
;; This counts newline.
#+allegro
(defmacro peeknext-char (stream)
  `(progn (skipbl ,stream) ; this is for newline
     (peek-char nil ,stream nil ,stream)))

#+sbcl
(defun peeknext-char (stream peek-type eof-error-p eof-value)
  (sb-impl::s-%peek-char stream peek-type eof-error-p eof-value))
#+allegro
(defmacro putback-char (c stream)
  `(unread-char ,c ,stream))
#+sbcl
(defun putback-char (c stream)
  (sb-impl::s-%unread-char (stream c)))
#+allegro
(defmacro getnext-char (stream)
  `(read-char ,stream))
#+sbcl
(defun getnext-char (stream  eof-error-p eof-value blocking-p)
  (sb-impl::s-%read-char (stream eof-error-p eof-value blocking-p)))

;;;
;;; Utilities
;;;

(defun null-line-p (line pos &optional (len (length line)))
;;;  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  (>= pos len))

(defun expose-line (pathname target-pos)
  ;; skip stuff until target-pos
  (cl:with-open-file (stream pathname)
    (cl:file-position stream target-pos)
    (cl:read-line stream)))

;; End of module
;; --------------------------------------------------------------------
