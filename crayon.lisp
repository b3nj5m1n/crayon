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

(defun crayon (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) args)))

(defmethod hex-to-rgb ((hex string))
  "Convert hexadecimal color string to list of rgb values
  "
  (if (string-equal "#" (subseq hex 0 1))
      (setf hex (subseq hex 1 (length hex))))
  (case (length hex)
    (3 (mapcar (lambda (e) (* 17 (parse-integer (subseq hex (* e 1) (+ 1 (* e 1))) :radix 16))) '(0 1 2)))
    (6 (mapcar (lambda (e) (parse-integer (subseq hex (* e 2) (+ 2 (* e 2))) :radix 16)) '(0 1 2)))
    (8 (mapcar (lambda (e) (parse-integer (subseq hex (* e 2) (+ 2 (* e 2))) :radix 16)) '(0 1 2)))))
