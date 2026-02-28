#lang info

(define collection "cabin-mixer")
(define racket-launcher-names '("cabin-mixer"))
(define racket-launcher-libraries '("gui/mixer"))
(define gracket-launcher-names '("CabinMixer"))
(define gracket-launcher-libraries '("gui/mixer"))
(define deps '("data-frame"
               "https://github.com/benknoble/frosthaven-manager.git"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "gui-easy-lib"
               "qi-lib"
               "sawzall-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/cabin-mixer.scrbl" ())))
(define pkg-desc "Cabin Mixer")
(define version "0.0")
(define pkg-authors '(benknoble))
(define license 'MIT)
