;;;; package.lisp

(defpackage #:chalk
  (:use #:cl)
  (:export bold
           faint
           italic
           underlined
           inverted
           crossed-out
           fg-red
           fg-green))
