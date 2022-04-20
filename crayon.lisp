;;;; crayon.lisp

(in-package #:crayon)

(defun escape-ansi (n &rest args)
  "Creates an ansi escape sequence

  Given n and optionally arguments, construct
  [SGR](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters)
  escape sequence.

  For example, to set the foreground color to the rgb value 255 0 0 (red),
  the n for setting the foreground color to an rgb value is 38, the next
  argument is a 2 to specify rgb values, followed by the rgb values as
  seperate parameters.

      ```
      (escape-ansi 38 2 255 0 0)
      ```
  "
  (if args
      (format nil "~c[~d;~am" #\ESC n (reduce (lambda (x y) (format nil "~d;~d" x y)) args))
      (format nil "~c[~dm" #\ESC n)))


;;; Setting

