;;;; chalk.lisp

(in-package #:chalk)

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

(defun escape-set-bold ()
  "Creates an ansi escape sequence for setting the font to bold
  "
  (escape-ansi 1))

(defun escape-set-faint ()
  "Creates an ansi escape sequence for setting the font to faint
  "
  (escape-ansi 2))

(defun escape-set-italic ()
  "Creates an ansi escape sequence for setting the font to italic
  "
  (escape-ansi 3))

(defun escape-set-underlined ()
  "Creates an ansi escape sequence for setting the font to underlined
  "
  (escape-ansi 4))

(defun escape-set-inverted ()
  "Creates an ansi escape sequence for setting the colors to inverted
  "
  (escape-ansi 7))

(defun escape-set-crossed-out ()
  "Creates an ansi escape sequence for setting the font to crossed-out
  "
  (escape-ansi 9))

(defun escape-set-rgb-fg (r g b)
  "Creates an ansi escape sequence for setting the foreground color to rgb value
  "
  (escape-ansi 38 2 r g b))

(defun escape-set-rgb-bg (r g b)
  "Creates an ansi escape sequence for setting the background color to rgb value
  "
  (escape-ansi 48 2 r g b))

;;; Resetting

(defun escape-reset-bold ()
  "Creates an ansi escape sequence for resetting the bold & faint attribute
  "
  (escape-ansi 22))

(defun escape-reset-faint ()
  "Creates an ansi escape sequence for resetting the bold & faint attribute
  "
  (escape-ansi 22))

(defun escape-reset-italic ()
  "Creates an ansi escape sequence for resetting the italic attribute
  "
  (escape-ansi 23))

(defun escape-reset-underlined ()
  "Creates an ansi escape sequence for resetting the underlined attribute
  "
  (escape-ansi 24))

(defun escape-reset-inverted ()
  "Creates an ansi escape sequence for resetting the inverted attribute
  "
  (escape-ansi 27))

(defun escape-reset-crossed-out ()
  "Creates an ansi escape sequence for resetting the crossed-out attribute
  "
  (escape-ansi 29))

(defun escape-reset-fg ()
  "Creates an ansi escape sequence for resetting the foreground color
  "
  (escape-ansi 39))

(defun escape-reset-bg ()
  "Creates an ansi escape sequence for resetting the background color
  "
  (escape-ansi 49))

;;; Text

(defun bold (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bold) (escape-set-bold) s2 (escape-reset-bold))) args :initial-value (escape-set-bold))))

(defun faint (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-faint) (escape-set-faint) s2 (escape-reset-faint))) args :initial-value (escape-set-faint))))

(defun italic (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-italic) (escape-set-italic) s2 (escape-reset-italic))) args :initial-value (escape-set-italic))))

(defun underlined (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-underlined) (escape-set-underlined) s2 (escape-reset-underlined))) args :initial-value (escape-set-underlined))))

(defun inverted (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-inverted) (escape-set-inverted) s2 (escape-reset-inverted))) args :initial-value (escape-set-inverted))))

(defun crossed-out (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-crossed-out) (escape-set-crossed-out) s2 (escape-reset-crossed-out))) args :initial-value (escape-set-crossed-out))))

(defun fg-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-rgb-fg 255 0 0) s2 (escape-reset-fg))) args :initial-value (escape-rgb-fg 255 0 0))))

(defun fg-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-rgb-fg 0 255 0) s2 (escape-reset-fg))) args :initial-value (escape-rgb-fg 0 255 0))))
