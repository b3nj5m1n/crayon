(in-package #:fg)

(defun rgb (r g b string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-rgb r g b) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-rgb r g b))))

(defun 8-bit (i string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-8-bit i) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-8-bit i))))

(defun black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-black) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-black))))
(defun red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-red) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-red))))
(defun green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-green) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-green))))
(defun yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-yellow) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-yellow))))
(defun blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-blue) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-blue))))
(defun magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-magenta) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-magenta))))
(defun cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-cyan) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-cyan))))
(defun white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-white) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-white))))

(defun bright-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-black) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-black))))
(defun bright-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-red) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-red))))
(defun bright-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-green) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-green))))
(defun bright-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-yellow) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-yellow))))
(defun bright-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-blue) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-blue))))
(defun bright-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-magenta) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-magenta))))
(defun bright-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-cyan) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-cyan))))
(defun bright-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-fg) (escape-set-fg-bright-white) s2 (escape-reset-fg))) args :initial-value (escape-set-fg-bright-white))))

;;; Resetting

(defun escape-reset-fg ()
  "Creates an ansi escape sequence for resetting the foreground color
  "
  (crayon:escape-ansi 39))

;;; Setting

(defun escape-set-fg-rgb (r g b)
  "Creates an ansi escape sequence for setting the foreground color to rgb value
  "
  (crayon:escape-ansi 38 2 r g b))

(defun escape-set-fg-8-bit (i)
  "Creates an ansi escape sequence for setting the foreground color using 8-bit
  lookup table
  "
  (crayon:escape-ansi 38 5 i))

(defun escape-set-fg-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (crayon:escape-ansi 30))
(defun escape-set-fg-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (crayon:escape-ansi 31))
(defun escape-set-fg-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (crayon:escape-ansi 32))
(defun escape-set-fg-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (crayon:escape-ansi 33))
(defun escape-set-fg-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (crayon:escape-ansi 34))
(defun escape-set-fg-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (crayon:escape-ansi 35))
(defun escape-set-fg-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (crayon:escape-ansi 36))
(defun escape-set-fg-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (crayon:escape-ansi 37))

(defun escape-set-fg-bright-black ()
  "Creates an ansi escape sequence for setting the foreground color to black
  "
  (crayon:escape-ansi 90))
(defun escape-set-fg-bright-red ()
  "Creates an ansi escape sequence for setting the foreground color to red
  "
  (crayon:escape-ansi 91))
(defun escape-set-fg-bright-green ()
  "Creates an ansi escape sequence for setting the foreground color to green
  "
  (crayon:escape-ansi 92))
(defun escape-set-fg-bright-yellow ()
  "Creates an ansi escape sequence for setting the foreground color to yellow
  "
  (crayon:escape-ansi 93))
(defun escape-set-fg-bright-blue ()
  "Creates an ansi escape sequence for setting the foreground color to blue
  "
  (crayon:escape-ansi 94))
(defun escape-set-fg-bright-magenta ()
  "Creates an ansi escape sequence for setting the foreground color to magenta
  "
  (crayon:escape-ansi 95))
(defun escape-set-fg-bright-cyan ()
  "Creates an ansi escape sequence for setting the foreground color to cyan
  "
  (crayon:escape-ansi 96))
(defun escape-set-fg-bright-white ()
  "Creates an ansi escape sequence for setting the foreground color to white
  "
  (crayon:escape-ansi 97))

