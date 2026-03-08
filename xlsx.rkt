#lang racket

(provide xlxs->df)

(require simple-xlsx
         csv-writing
         data-frame)

(define (xlxs->df f)
  (define-values (read-end write-end) (make-pipe))
  (display-table (get-sheet0-data f) write-end)
  (close-output-port write-end)
  (define df (df-read/csv read-end))
  df)

(define (get-sheet0-data f)
  (define rows #f)
  (read-xlsx f (thunk (set! rows (with-sheet get-rows))))
  rows)
