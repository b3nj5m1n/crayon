(in-package #:bg)

(defun gradient (gradient-args string)
  (let* ((len (1- (length string)))
         (args (concatenate 'list (list len) gradient-args))
         (result ""))
    (loop for color in (apply #'crayon:gradient-steps args) and idx from 0 do
          (setq result (concatenate 'string result (rgb color (subseq string idx (1+ idx))))))
    result))

(defmethod rgb ((rgb list) string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (destructuring-bind (r g b) rgb
      (rgb-p r g b args))))
(defmethod rgb ((rgb string) string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (destructuring-bind (r g b) (crayon:hex-to-rgb rgb)
      (rgb-p r g b args))))

(defun rgb-p (r g b args)
  (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-rgb r g b) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-rgb r g b)))

(defun 8-bit (i string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-8-bit i) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-8-bit i))))

(defun black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-black) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-black))))
(defun red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-red) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-red))))
(defun green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-green) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-green))))
(defun yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-yellow) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-yellow))))
(defun blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-blue) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-blue))))
(defun magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-magenta) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-magenta))))
(defun cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-cyan) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-cyan))))
(defun white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-white) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-white))))

(defun bright-black (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-black) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-black))))
(defun bright-red (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-red) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-red))))
(defun bright-green (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-green) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-green))))
(defun bright-yellow (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-yellow) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-yellow))))
(defun bright-blue (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-blue) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-blue))))
(defun bright-magenta (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-magenta) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-magenta))))
(defun bright-cyan (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-cyan) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-cyan))))
(defun bright-white (string &rest strings)
  (let ((args (concatenate 'list (list string) strings)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 (escape-reset-bg) (escape-set-bg-bright-white) s2 (escape-reset-bg))) args :initial-value (escape-set-bg-bright-white))))

;;; Resetting

(defun escape-reset-bg ()
  "Creates an ansi escape sequence for resetting the background color
  "
  (escape-ansi 49))

;;; Setting

(defun escape-set-bg-rgb (r g b)
  "Creates an ansi escape sequence for setting the background color to rgb value
  "
  (escape-ansi 48 2 r g b))

(defun escape-set-bg-8-bit (i)
  "Creates an ansi escape sequence for setting the background color using 8-bit
  lookup table
  "
  (escape-ansi 48 5 i))

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
