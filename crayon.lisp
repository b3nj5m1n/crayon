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


(defun gradient-interpolate (steps args)
    (let ((l '(1 2 3))
          (result (list)))
        (loop for i from 1 to (1- (length l)) do
              (let* ((color1 (nth (1- i) args))
                     (color2 (nth i args))
                     (1_r (first (second color1)))
                     (1_g (second (second color1)))
                     (1_b (third (second color1)))
                     (2_r (first (second color2)))
                     (2_g (second (second color2)))
                     (2_b (third (second color2)))
                     (steps_color (floor (* steps (- (first color2) (first color1)))))
                     (difference_r (- 2_r 1_r))
                     (difference_g (- 2_g 1_g))
                     (difference_b (- 2_b 1_b))
                     (delta_r (float (/ difference_r steps_color)))
                     (delta_g (float (/ difference_g steps_color)))
                     (delta_b (float (/ difference_b steps_color))))
                  ; (format t "Initial color => red: ~s green: ~s blue: ~s~%" 1_r 1_g 1_b)
                  ; (format t "New color => red: ~s green: ~s blue: ~s~%" 2_r 2_g 2_b)
                  ; (format t "This gradient is going to occupy ~s steps~%" steps_color)
                  ; (format t "Diffs => red: ~s green: ~s blue: ~s~%" difference_r difference_g difference_b)
                  ; (format t "Deltas => red: ~s green: ~s blue: ~s~%" delta_r delta_g delta_b)
                  ; (format t "~%")
                  (loop for j from 0 to steps_color do
                        (setq result (append result (list (list
                                                            (floor (+ (* delta_r j) 1_r))
                                                            (floor (+ (* delta_g j) 1_g))
                                                            (floor (+ (* delta_b j) 1_b)))))))))
        result))

; (gradient-interpolate 20 (gradient 20 "38BDF8" "FB7185" "A3E635"))

; (third (second '(0.5 (251 113 133))))

(defmethod gradient-steps ((steps integer) (c1 list) (c2 list) &rest r)
  "Gradient variant for even distribution with rgb values
  "
  (let* ((colors (concatenate 'list (list c1 c2) r))
         (step (float (/ 1 (1- (length colors)))))
         (args (loop for color in colors and idx from 0 collect (list (* idx step) color))))
    (gradient-interpolate steps args)))
(defmethod gradient-steps ((steps integer) (c1 string) (c2 string) &rest r)
  "Gradient variant for even distribution with hex values
  "
  (let* ((colors (mapcar 'hex-to-rgb (concatenate 'list (list c1 c2) r)))
         (step (float (/ 1 (1- (length colors)))))
         (args (loop for color in colors and idx from 0 collect (list (* idx step) color))))
    (gradient-interpolate steps args)))
(defmethod gradient-steps ((steps integer) (s1 float) (c1 list) &rest r)
  "Gradient variant for defined distribution with rgb values
  "
  (let* ((args (concatenate 'list (list s1 c1) r))
         (args (loop for (idx color) on args by #'cddr collect (list idx color))))
    (gradient-interpolate steps args)))
(defmethod gradient-steps ((steps integer) (s1 float) (c1 string) &rest r)
  "Gradient variant for defined distribution with hex values
  "
  (let* ((args (concatenate 'list (list s1 c1) r))
         (args (loop for (idx color) on args by #'cddr collect (list idx (hex-to-rgb color)))))
    (gradient-interpolate steps args)))

; ; Variant 1
; (gradient 20 '(56 189 248) '(251 113 133) '(163 230 53))
; ; Variant 2
; (gradient 20 "38BDF8" "FB7185" "A3E635")
; ; Variant 3
; (gradient 20 0.0 '(56 189 248) 0.5 '(251 113 133) 0.5 '(163 230 53))
; ; Variant 4
; (gradient 20 0.0 "38BDF8" 0.5 "FB7185" 0.5 "A3E635")

