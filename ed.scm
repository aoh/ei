#!/usr/bin/ol --run

;;;
;;; Ed - an implementation of the standard editor
;;;

(import
   (owl parse)
   (owl args))

(define version "0.1a")

(define (cut lst thing)
   (let loop ((lst lst) (this null) (others null))
      (cond
         ((null? lst)
            (if (null? this)
               (reverse others)
               (reverse (cons (reverse this) others))))
         ((eq? (car lst) thing)
            (loop (cdr lst) null (cons (reverse this) others)))
         (else
            (loop (cdr lst) (cons (car lst) this) others)))))

(define command-line-rule-exp
 `((help "-h" "--help")
   (about "-A" "--about")
   (version "-V" "--version")
   (prompt "-p" "--prompt" has-arg
      comment "optional prompt")
   ))

(define command-line-rules 
   (cl-rules command-line-rule-exp))

(define (upto-dot-line)
   (either
      (let-parses
         ((skip (imm #\newline))
          (skip (imm #\.))
          (skip (imm #\newline)))
         null)
      (let-parses
         ((r rune)
          (rs (upto-dot-line)))
         (cons r rs))))

(define (get-action range)
   (any
      (let-parses
         ((skip (imm #\a))
          (skip (imm #\newline))
          (cs (upto-dot-line)))
         (tuple 'append range cs))
      (let-parses
         ((skip (imm #\u)))
         (tuple 'undo))
      (let-parses
          ((skip (imm #\=)))
          (tuple 'print-position range))
      (let-parses
          ((skip (imm #\n)))
          (tuple 'print range #t))
      (let-parses
          ((skip (either (imm #\p) (imm #\newline)))) ; default op is print
          (tuple 'print range #f))
       ))
 
(define get-digit
   (get-byte-if
      (λ (x) (<= #\0 x #\9))))

(define get-natural
   (let-parses
      ((digits (plus get-digit)))
      (fold
         (λ (n c) (+ (* n 10) (- c #\0)))
         0 digits)))

(define special-positions
   (list->ff 
      '((#\. . dot)
        (#\$ . end))))

(define special-ranges
   (list->ff
      (list
         (cons #\% (tuple 'range 'start 'end))
         (cons #\; (tuple 'range 'dot 'end)))))
        
(define get-special-place
   (let-parses
      ((b (get-byte-if (λ (x) (special-positions x #f)))))
      (special-positions b)))

(define get-special-range
   (let-parses
      ((b (get-byte-if (λ (x) (special-ranges x #f)))))
      (special-ranges b)))

(define get-mark
   (let-parses
      ((skip (get-imm #\'))
       (id get-rune))
      (tuple 'mark id)))

(define get-position
   (any
      get-natural
      get-special-place
      get-mark
      (get-epsilon 'default)))

(define get-range
   (either
      (let-parses
         ((start get-position)
          (end 
             (either
               (let-parses
                  ((skip (get-imm #\,))
                   (end get-position))
                  end)
               (epsilon #false))))
         (if end
            (tuple 'range start end)
            start))
      get-special-range))

(define get-whitespace
   (get-byte-if
      (λ (x) (or (eq? x #\newline)
                 (eq? x #\space)))))
     
(define maybe-whitespace
   (let-parses
      ((skip (star get-whitespace)))
      'whitespace))

(define get-command 
   (let-parses
      ((skip maybe-whitespace)
       (range get-range)
       (action (get-action range)))
      action))

(define usage-text 
   "Usage: ed [args] [path]")

(define (eval-position env u d l exp default)
   (cond
      ((number? exp) exp)
      ((eq? exp 'dot) l)
      ((eq? exp 'start) 1)
      ((eq? exp 'end) (+ l (length d)))
      ((eq? exp 'default)
         (eval-position env u d l default default))
      (else
         #false)))

(define (eval-range env u d l exp default)
   (cond
      ((eq? exp 'default)
         (eval-range env u d l default default))
      ((eval-position env u d l exp 'dot) =>
         (λ (pos)
            ;; unit range
            (values pos pos)))
      ((tuple? exp)
         (tuple-case exp
            ((range from to)
               (lets ((start (eval-position env u d l from 'start))
                      (end   (eval-position env u d l to   'end)))
                  (if (and start end)
                     (values start end)
                     (values #f #f))))
            (else
               (print-to stderr (str "range wat: " exp))
               (values #f #f))))
      (else
         (print-to stderr (str "range wat: " exp))
         (values #f #f))))

(define (seek-line u d l n)
   (cond
      ((= l n)
         (values u d l))
      ((< n l)
         (if (null? u)
            (values #f #f #f)
            (seek-line (cdr u) (cons (car u) d) (- l 1) n)))
      ((null? d)
         (values #f #f #f))
      (else
         (seek-line (cons (car d) u) (cdr d) (+ l 1) n))))

(define (print-range env u d l to number?)
   (print (if number? (str l "   ") "") (runes->string (car u)))
   (cond
      ((= l to)
         (values u d l))
      ((null? d)
         ;; or fail
         (values u d l))
      (else
         (print-range env (cons (car d) u) (cdr d) (+ l 1) to number?))))

(define (valid-position? pos dot get-end)
   (cond
      ((< pos 1)
         #false)
      ((<= pos dot)
         #true)
      ((> pos (get-end)) ; O(n) atm
         #false)
      (else #true)))

(define (valid-range? from to dot get-end)
   (and
      (valid-position? from dot get-end)
      (valid-position? to   dot get-end)
      (<= from to)))

(define (maybe-prompt env)
   (let ((prompt (get env 'prompt #f)))
      (if prompt
         (display prompt))))

;; dot is car of u, l is length of u
(define (ed es env u d l)
   (maybe-prompt env)
   (lets ((a es (uncons es #f)))
      ;(print-to stderr a)
      (if a
         (tuple-case a
            ((append pos data)
               (if (null? data)
                  (ed es env u d l)
                  (lets ((lines (cut data #\newline)))
                     (ed es 
                        (put env 'undo (tuple u d l))
                        (append (reverse lines) u)
                        d
                        (+ l (length lines))))))
            ((undo)
               (let ((last (getf env 'undo)))
                  (if last
                     (lets ((lu ld ll last))
                        (ed es
                           (put env 'undo (tuple u d l))
                           lu ld ll))
                     (begin
                        (print-to stderr "!")
                        (ed es env u d l)))))
            ((print-position range)
               (lets ((pos (eval-position env u d l range 'dot)))
                  (print pos)
                  (ed es env u d l)))
            ((print range number?)
               (lets ((from to (eval-range env u d l range 'dot)))
                  (if (valid-range? from to l (λ () (+ l (length d))))
                     (lets
                        ((u d l (seek-line u d l from))
                         (u d l (print-range env u d l to number?)))
                        (ed es env u d l))
                     (begin
                        (print-to stderr "?")
                        (ed es env u d l)))))
            (else
               (print-to stderr "?!")
               (ed es env u d l)))
         (print "bye"))))

(define (forward-read ll)
   (if (pair? ll)
      (forward-read (cdr ll))
      ll))

(define (syntax-error-handler recurse ll message)
   (print-to stderr (str "?"))
   (recurse (forward-read ll)))

(define (start-ed dict args)
   (cond
      ((getf dict 'about) 
         (print "about what")
         0)
      ((getf dict 'version)
         (print "ed v" version)
         0)
      ((getf dict 'help)
         (print usage-text)
         (print (format-rules command-line-rules))
         0)
      (else
         (ed 
            (λ () (fd->exp-stream stdin get-command syntax-error-handler))
            dict null null 0))))

(define (main args)
   (process-arguments args command-line-rules usage-text start-ed))

main

