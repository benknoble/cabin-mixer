#lang racket

(module+ main
  (require racket/gui/easy/debugger
           (prefix-in dbg: debugging/server))
  (define data-file
    (command-line
     #:once-each
     [("--debug") "Enable debugging"
                  (start-debugger)
                  (dbg:serve)]
     #:args ([data-csv-file #f]) data-csv-file))
  ;; (delete-current-assignments! data)
  (define has-fs-change?
    (match (system-type 'fs-change)
      [(vector 'supported _ _ _) #t]
      [_
       (define-close! close! closing-mixin)
       (render
        (dialog
         #:title "Cabin Mixer"
         #:mixin closing-mixin
         (text "Your system does not support reacting to changes in files.")
         (text "To update charts displayed by Cabin Mixer, use 'File' > 'Open' and choose a data file.")
         (button "Confirm" (thunk (close!)))))
       #f]))
  (define-values (@data fsc-data)
    (cond
      [(not data-file) (define/obs @data (make-data-frame))
                       (values @data (fs-change #f (thread void) @data))]
      [has-fs-change? (define fsc-data (start-fs-change data-file))
                      (values (fs-change-@data fsc-data) fsc-data)]
      [else (values (@ (read-file data-file)) #f)]))
  (define error-logs
    (cond
      [(terminal-port? (current-error-port))
       (define temp (make-temporary-file "cabin-mixer-~a"))
       (current-error-port (open-output-file temp #:exists 'truncate #:mode 'text))
       temp]
      [else #f]))
  (void (render (mixer @data error-logs
                       #:open-data-file
                       (λ (file)
                         (when file
                           (:= @data (read-file file))
                           (when fsc-data
                             (restart-fs-change fsc-data file))))))))

(require cabin-mixer/gui/common-menu
         cabin-mixer/xlsx
         racket/gui/easy
         frosthaven-manager/curlique
         frosthaven-manager/observable-operator
         frosthaven-manager/files
         frosthaven-manager/gui/mixins
         plot
         data-frame)

(define modifier
  (case (system-type 'os)
    [(macosx) 'cmd]
    [else 'ctl]))

#|

 Chart | discovered
-------| fields
 Table | toggles

 Maybe assignment table should be separate window?
 Maybe "field toggles" should be a "preferences" window?

|#
(define (mixer @data error-logs
               #:open-data-file open-data-file)
  (define/obs @chart #f)
  (define (init-@chart!)
    (define chart (@! @chart))
    (define names (df-series-names (@! @data)))
    (unless (and chart (member chart names))
      (:= @chart
          (match names
            [(cons x _) x]
            [_ #f]))))
  (init-@chart!)
  (define/obs @style 'count)
  (define (open-data)
    (open-data-file (get-file/filter "Data file" '("CSV/Spreadsheet" "*.csv;*.xlsx;*.xls")))
    (init-@chart!))
  (define-close! close! closing-mixin)
  (window
   #:title "Cabin Mixer"
   #:mixin closing-mixin
   (menu-bar
    (menu "File"
          (menu-item "&New Window" (thunk (void (render (mixer @data error-logs #:open-data-file open-data-file))))
                     #:shortcut (list modifier #\n))
          (menu-item "&Open Data" open-data
                     #:shortcut (list modifier #\o))
          (menu-item "Close &Window" (thunk (close!))
                     #:shortcut (list modifier #\w))
          (menu-item "&Quit" (thunk (exit 0))
                     #:shortcut (list modifier #\q)))
    (menu "Debug"
          (error-logs-menu-item error-logs))
    (menu "Help"
          (about-menu-item)
          (documentation-menu-item)
          (menu-item-separator)
          (send-feedback-menu-item)
          (issue-menu-item)
          (feature-menu-item)
          (contribute-menu-item)))
   (choice '(count proportion)
           #:label "Chart type: "
           #:choice->label {~> ~a string-titlecase}
           #:selection @style
           (λ (new-choice)
             (:= @style (or new-choice 'count))))
   (tabs
    (@> @data df-series-names)
    (λ (e _choices current)
      (case e [(select) (:= @chart current)]))
    (observable-view
     (obs-combine vector @data @chart @style)
     (match-lambda
       [(vector data chart style)
        (cond
          [(zero? (length (df-series-names data)))
           (hpanel
            #:alignment '(center center)
            (button "Open data" open-data))]
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
                 #:x-label "Cabin #"
                 #:y-label (format "~a by ~a"
                                   (match style
                                     ['count "Count"]
                                     ['proportion "Percentage"])
                                   chart)
                 (stacked-histogram chart-data #:labels the-labels)))))]
          [else
           (hpanel
            #:alignment '(center center)
            (text "Please select a chart from the list of tabs."))])])))))

(define (read-file file)
  (or
   (with-handlers ([exn:fail? {#f}])
     (xlxs->df file))
   (with-handlers ([exn:fail? {#f}])
     ;; HMPH! This doesn't fail…
     ;; nor does the csv->list stuff from csv-reading…
     ;; so for now, read-file-error will never be seen?
     (df-read/csv file))
   (read-file-error)))

(define (read-file-error)
  (define-close! close! closing-mixin)
  (render
   (dialog
    #:title "Cabin Mixer: Read Error"
    #:mixin closing-mixin
    (text "The selected file was unreadable.")
    (text "Supported file types: CSV, XLSX.")
    (button "OK" (thunk (close!)))))
  (make-data-frame))

(struct fs-change [file thd @data] #:mutable)

(define (start-fs-change file)
  (define/obs @data (read-file file))
  (fs-change
   file
   (thread (thunk (run-fs-change file @data)))
   @data))

(define (restart-fs-change fsc-data file)
  (match-define (fs-change _ thd @data) fsc-data)
  (kill-thread thd)
  (set-fs-change-file! fsc-data file)
  (set-fs-change-thd! fsc-data (thread (thunk (run-fs-change file @data)))))

(define (run-fs-change file @data)
  (let go ()
    (sync (filesystem-change-evt file))
    ;; some editors (Vim) write to a new file and move it into place
    (let wait-for-file ()
      (cond
        [(file-exists? file) (:= @data (read-file file))]
        [else (wait-for-file)]))
    (go)))

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
