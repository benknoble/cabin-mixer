#lang racket

(module+ main
  (void
   (render
    (window
     #:title "Cabin Mixer"
     (text "Coming Soon")))))

(require racket/gui/easy
         sawzall
         graphite
         qi)

(module+ test
  (require rackunit)
  (test-equal? "Getting Started" 1 1))
