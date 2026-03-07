#lang racket

(provide
  (contract-out
    [do-about (-> renderer?)]
    [about-menu-item (-> (is-a?/c view<%>))]
    [issue-menu-item (-> (is-a?/c view<%>))]
    [feature-menu-item (-> (is-a?/c view<%>))]
    [contribute-menu-item (-> (is-a?/c view<%>))]
    [send-feedback-menu-item (-> (is-a?/c view<%>))]
    [documentation-menu-item (-> (is-a?/c view<%>))]
    [logs-widget (-> (or/c #f path?) (is-a?/c view<%>))]
    [error-logs-menu-item (-> (or/c #f path?) (is-a?/c view<%>))]))

(require frosthaven-manager/gui/markdown
         frosthaven-manager/observable-operator
         net/sendurl
         racket/gui/easy
         racket/runtime-path)

(module+ test (require rackunit))

(begin-for-syntax
  (require setup/getinfo)
  (define (get-version) ((get-info '("cabin-mixer")) 'version)))
(define-syntax (version stx)
  (datum->syntax stx (get-version) stx stx))

(module+ test
  (require version/utils)
  (define-simple-check (check-valid-version x)
                       (valid-version? x))
  (check-valid-version version))

(define-runtime-path about.md "../ABOUT.md")

(define (get-about-text)
  (define about-text (file->string about.md))
  (string-join (list about-text "---" (string-append "Version: " version)) "\n"))

(module+ test
  (test-case "about information"
    (check-not-exn get-about-text)
    (check-match (get-about-text) (regexp #px"Version: (.*)" (list _ the-version))
                 (valid-version? the-version))))

(define (do-about)
  (render
   (window
    #:title "About Cabin Mixer"
    #:size '(400 300)
    (markdown-text (get-about-text)))))

(define (about-menu-item)
  (menu-item "About Cabin Mixer" do-about))

(define (issue-menu-item)
  (menu-item
    "Report an Issue"
    (thunk (send-url "https://github.com/benknoble/cabin-mixer/issues/new/choose"))))

(define (feature-menu-item)
  (menu-item
    "Request a Feature"
    (thunk (send-url "https://github.com/benknoble/cabin-mixer/issues/new/choose"))))

(define (contribute-menu-item)
  (menu-item
    "Contribute to development"
    (thunk (send-url "https://github.com/benknoble/cabin-mixer"))))

(define (send-feedback-menu-item)
  (menu-item
    "Send Feedback"
    (thunk (send-url "mailto:ben.knoble+cabin-mixer@gmail.com"))))

(define (documentation-menu-item)
  (menu-item "Documentation" (thunk (send-url "https://benknoble.github.io/cabin-mixer"))))

(define (error-logs-menu-item error-logs)
  (menu-item
   "Show error logs"
   (thunk
    (render
      (window
       #:title "Error logs"
       (logs-widget error-logs))))))

(define (logs-widget error-logs)
  (text (~> (error-logs)
            (if _ path->string "standard error on your terminal device")
            (~a "Errors logs are written to " _ "."))))

(module+ main
  (render
    (window
      (text "Check menus")
      (menu-bar
        (menu "Help"
              (about-menu-item)
              (documentation-menu-item)
              (menu-item-separator)
              (send-feedback-menu-item)
              (issue-menu-item)
              (feature-menu-item)
              (contribute-menu-item))))))
