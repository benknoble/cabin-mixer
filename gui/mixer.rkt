#lang racket

(module+ main
  (define data
    (command-line
     #:args (data-csv-file)
     (df-read/csv data-csv-file)))
  ;; (delete-current-assignments! data)
  #|

   Chart | discovered
  -------| fields
   Table | toggles

   Maybe assignment table should be separate window?
   Maybe "field toggles" should be a "preferences" window?

  |#
  (define/obs @chart #f)
  (define/obs @style 'count)
  (plot-pen-color-map 'set3)
  (plot-brush-color-map 'set3)
  (void
   (render
    (window
     #:title "Cabin Mixer"
     (choice '(count proportion)
             #:choice->label ~a
             #:selection @style
             (λ (new-choice)
               (:= @style (or new-choice 'count))))
     (tabs
      (df-series-names data)
      (λ (e _choices current)
        (case e [(select) (:= @chart current)]))
      (observable-view
       ;; NB data may be observable here, too. Should probably use a single
       ;; @state or obs-combine them here
       (obs-combine vector @chart @style)
       (match-lambda
         [(vector chart style)
          (cond
            [chart
             (snip
              data
              (λ (data w h)
                (define label (add-labels! data chart))
                (define the-data (~> (data) (df-select* "Cabin" label) vector->list (map vector->list _)))
                (define the-labels (~> (data) (df-select label)
                                       vector->list sep set set->list
                                       (sort string<?)))
                (define count-data
                  (sort (for/list ([group (group-by first the-data)])
                          (define cabin (first (first group)))
                          (define counter
                            (for/fold ([acc (hash)])
                                      ([value (map second group)])
                              (hash-update acc value add1 0)))
                          (list cabin
                                (for/list ([label the-labels])
                                  (hash-ref counter label 0))))
                        string<?
                        #:key {~> first ~a}))
                (define chart-data
                  (case style
                    [(count) count-data]
                    [(proportion)
                     (for/list ([group count-data])
                       (define cabin (first group))
                       (define counts (second group))
                       (define total (apply + counts))
                       (list cabin (map {(/ total)} counts)))]))
                (plot-snip
                 #:width w
                 #:height h
                 (stacked-histogram chart-data #:labels the-labels))))]
            [else (text "No chart selected")])])))))))

(require racket/gui/easy
         frosthaven-manager/curlique
         frosthaven-manager/observable-operator
         sawzall
         plot
         plot/snip
         (prefix-in pict: pict)
         data-frame)

(define (delete-current-assignments! df)
  (for ([series (df-series-names df)]
        #:when (regexp-match? #px"(?i:cabin|discussion)" series))
    (df-del-series! df series)))

(define (add-labels! df series)
  (define label (format "~a Labels" series))
  (df-add-derived! df label (list series)
                   {~> car (or _ "unknown") ~a})
  label)

(module+ test
  (require rackunit)
  (test-equal? "Getting Started" 1 1))
