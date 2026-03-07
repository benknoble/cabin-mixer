#lang racket

(module+ main
  (require racket/gui/easy/debugger
           (prefix-in dbg: debugging/server))
  (define data
    (command-line
     #:once-each
     [("--debug") "Enable debugging"
                  (start-debugger)
                  (dbg:serve)]
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
  (void (render (mixer data))))

(require racket/gui/easy
         racket/gui/base
         frosthaven-manager/curlique
         frosthaven-manager/observable-operator
         sawzall
         plot
         plot/snip
         (prefix-in pict: pict)
         data-frame)

(define modifier
  (case (system-type 'os)
    [(macosx) 'cmd]
    [else 'ctl]))

(define (mixer data)
  (define/obs @chart (match (df-series-names data)
                       [(cons x _) x]
                       [_ #f]))
  (define/obs @style 'count)
  (window
   #:title "Cabin Mixer"
   (menu-bar
    (menu "File"
          (menu-item "&New Window" (thunk (void (render (mixer data))))
                     #:shortcut (list modifier #\n))
          (menu-item "&Open Data" (thunk (raise "not implemented yet"))
                     #:shortcut (list modifier #\o))
          (menu-item "&Quit" (thunk ((application-quit-handler)))
                     #:shortcut (list modifier #\q)))
    (menu "Debug")
    (menu "Help"))
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
              (parameterize ([plot-pen-color-map 'tab20]
                             [plot-brush-color-map 'tab20])
              (plot-snip
               #:width w
               #:height h
               (stacked-histogram chart-data #:labels the-labels)))))]
          [else (text "Please select a chart from the list of tabs.")])])))))

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
