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

(defun escape-set-fg-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (escape-ansi 30))
(defun escape-set-fg-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (escape-ansi 31))
(defun escape-set-fg-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (escape-ansi 32))
(defun escape-set-fg-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (escape-ansi 33))
(defun escape-set-fg-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (escape-ansi 34))
(defun escape-set-fg-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (escape-ansi 35))
(defun escape-set-fg-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (escape-ansi 36))
(defun escape-set-fg-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (escape-ansi 37))

(defun escape-set-bg-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (escape-ansi 40))
(defun escape-set-bg-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (escape-ansi 41))
(defun escape-set-bg-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (escape-ansi 42))
(defun escape-set-bg-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (escape-ansi 43))
(defun escape-set-bg-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (escape-ansi 44))
(defun escape-set-bg-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (escape-ansi 45))
(defun escape-set-bg-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (escape-ansi 46))
(defun escape-set-bg-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (escape-ansi 47))

(defun escape-set-fg-bright-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (escape-ansi 90))
(defun escape-set-fg-bright-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (escape-ansi 91))
(defun escape-set-fg-bright-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (escape-ansi 92))
(defun escape-set-fg-bright-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (escape-ansi 93))
(defun escape-set-fg-bright-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (escape-ansi 94))
(defun escape-set-fg-bright-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (escape-ansi 95))
(defun escape-set-fg-bright-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (escape-ansi 96))
(defun escape-set-fg-bright-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (escape-ansi 97))

(defun escape-set-bg-bright-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (escape-ansi 100))
(defun escape-set-bg-bright-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (escape-ansi 101))
(defun escape-set-bg-bright-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (escape-ansi 102))
(defun escape-set-bg-bright-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (escape-ansi 103))
(defun escape-set-bg-bright-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (escape-ansi 104))
(defun escape-set-bg-bright-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (escape-ansi 105))
(defun escape-set-bg-bright-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (escape-ansi 106))
(defun escape-set-bg-bright-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (escape-ansi 107))

(defun escape-set-fg-8-bit (i)
  "Creates an ansi escape sequence for setting the foreground color using 8-bit
  lookup table
  "
  (escape-ansi 38 5 i))
(defun escape-set-bg-8-bit (i)
  "Creates an ansi escape sequence for setting the background color using 8-bit
  lookup table
  "
  (escape-ansi 48 5 i))

(defun escape-set-fg-rgb (r g b)
  "Creates an ansi escape sequence for setting the foreground color to rgb value
  "
  (escape-ansi 38 2 r g b))
(defun escape-set-bg-rgb (r g b)
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

(defun fg-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-black) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-black))))
(defun fg-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-red) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-red))))
(defun fg-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-green) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-green))))
(defun fg-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-yellow) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-yellow))))
(defun fg-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-blue) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-blue))))
(defun fg-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-magenta) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-magenta))))
(defun fg-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-cyan) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-cyan))))
(defun fg-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-white) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-white))))

(defun bg-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-black) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-black))))
(defun bg-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-red) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-red))))
(defun bg-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-green) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-green))))
(defun bg-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-yellow) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-yellow))))
(defun bg-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-blue) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-blue))))
(defun bg-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-magenta) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-magenta))))
(defun bg-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-cyan) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-cyan))))
(defun bg-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-white) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-white))))

(defun fg-bright-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-black) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-black))))
(defun fg-bright-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-red) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-red))))
(defun fg-bright-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-green) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-green))))
(defun fg-bright-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-yellow) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-yellow))))
(defun fg-bright-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-blue) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-blue))))
(defun fg-bright-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-magenta) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-magenta))))
(defun fg-bright-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-cyan) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-cyan))))
(defun fg-bright-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-white) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-white))))

(defun bg-bright-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-black) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-black))))
(defun bg-bright-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-red) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-red))))
(defun bg-bright-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-green) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-green))))
(defun bg-bright-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-yellow) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-yellow))))
(defun bg-bright-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-blue) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-blue))))
(defun bg-bright-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-magenta) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-magenta))))
(defun bg-bright-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-cyan) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-cyan))))
(defun bg-bright-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-white) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-white))))

(defun fg-8-bit (i string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-8-bit i) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-8-bit i))))
(defun bg-8-bit (i string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-8-bit i) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-8-bit i))))

(defun fg-rgb (r g b string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-rgb r g b) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-rgb r g b))))
(defun bg-rgb (r g b string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-rgb r g b) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-rgb r g b))))
