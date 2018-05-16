#!/usr/bin/ol --run

;;;
;;; Ed - an implementation of the standard editor
;;;

(import
   (owl parse)
   (owl unicode)
   (owl args))

;; line = (code-point ...)
;;      | (metadata-ff code-point ...)

(define (drop-line-info x)
   (cond
      ((null? x) x)
      ((ff? (car x))
         (cdr x))
      (else x)))

(define (clear-infos lines)
   (map drop-line-info lines))
      
(define (output env thing)
   (if (not (get env 'silent #f))
      (print thing)))

(define (complain env reason)
   (print "?")
   (if (get env 'verbose-complain #f)
      (print reason)))
      
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
 `((help "-h" "--help"
      comment "show this thing")
   (about "-A" "--about"
      comment "tell a bit about this program")
   (version "-V" "--version"
      comment "show program version")
   (silent "-s" "--silent"
      comment "save paper by not printing byte counts")
   (autocontext #false "--autocontext"
      comment "automatic .x after commands (temp)")
   (prompt "-p" "--prompt" has-arg
      comment "optional prompt")))

(define (modified env)
   (if (getf env 'modified)
      env
      (put env 'modified #true)))

(define (not-modified env)
   (del env 'modified))

(define (modified? env)
   (get env 'modified #false))
      

;; env:
;;   silent (bool), print write counts
;;   verbose-complain, bool, print multibyte error messages (wastes paper!)

(define command-line-rules
   (cl-rules command-line-rule-exp))

(define (upto-line delim)
   (either
      (let-parses
         ((skip (imm #\newline))
          (skip (word delim null))
          (skip (imm #\newline)))
         null)
      (let-parses
         ((r rune)
          (rs (upto-line delim)))
         (cons r rs))))

(define get-non-newline
   (get-rune-if
      (λ (x) (not (eq? x #\newline)))))

(define get-same-line-whitespace
   (get-rune-if
      (λ (x)
         (or (eq? x #\tab)
             (eq? x #\return)
             (eq? x #\space)))))

(define multiline-string-ops
   (-> #empty
      (put #\a 'append)
      (put #\c 'change)))

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

(define get-regex-search
   (let-parses
      ((delim (get-byte-if (λ (x) (or (eq? x #\/) (eq? x #\?)))))
       ;; owl regexps are extended. implement simple ones here later.
       ;; todo: the delimiter is actually arbitrary, at least in s?
       (chars (star (get-byte-if (λ (x) (not (eq? x delim))))))
       (skip (get-imm delim)))
      (tuple 
         'regex 
         (if (eq? delim #\?) 'up 'down)
         chars)))

(define get-leaf-position
   (one-of
      get-natural
      get-special-place
      get-mark
      get-regex-search
      (get-epsilon 'default)))

(define (ival byte x)
   (let-parses
      ((skip (imm byte)))
      x))

(define get-position
   (let-parses
      ((a get-leaf-position)
       (val
         (either
            (let-parses
               ((op (either (ival #\+ 'plus) (ival #\- 'minus)))
                (arg get-leaf-position))
               (tuple op a arg))
            (epsilon a))))
      val))

;; what is considered to be whitespace between commands in a g/re/ command list
;; the command list is terminated by a lone newline
(define get-global-whitespace
   (one-of
      (get-imm #\space)
      (get-imm #\tab)
      (let-parses
         ((skip (get-imm #\\))
          (skip (get-imm #\newline)))
         'line)))
      
(define (get-action range make-get-command)
   (one-of
      (let-parses
         ((op-char (get-byte-if (λ (x) (get multiline-string-ops x #false))))
          (delim (star get-non-newline)) ;; optional alternative delimiting line
          (skip (imm #\newline))
          (cs (upto-line (list->string (if (null? delim) (list #\.) delim)))))
         (tuple 
            (getf multiline-string-ops op-char)
            range cs))
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
         ((skip (imm #\d)))
         (tuple 'delete range))
      (let-parses
         ((skip (imm #\P))
          (prompt (star get-non-newline))) ;; optional multi-character prompt support!
         (tuple 'prompt 
            (list->string prompt)))
      (let-parses
         ((skip (imm #\w))
          (skip (star get-same-line-whitespace))
          (path (star get-non-newline))) ;; empty = last
         (tuple 'write range (list->string path)))
      (let-parses
         ((skip (imm #\m))
          (target get-position))
         (tuple 'move range target))
      (let-parses
         ((op (get-byte-if (λ (x) (or (eq? x #\e) (eq? x #\E)))))
          (skip (star get-same-line-whitespace))
          (path (star get-non-newline))) ;; empty = last
         (tuple 'edit 
            (list->string path)
            (eq? op #\E)))
      (let-parses
         ((skip (imm #\f))
          (skip (star get-same-line-whitespace))
          (path (star get-non-newline)))
         (tuple 'facts (if (null? path) #false (list->string path))))
      (let-parses
          ((skip (imm #\p)))
          (tuple 'print range #f))
      (let-parses
         ((skip (imm #\k))
          (tag get-rune))
         (tuple 'mark range tag))
      (let-parses
         ((skip (imm #\Q)))
         (tuple 'quit #true))
      (let-parses
         ((skip (imm #\q)))
         (tuple 'quit #false))
      (let-parses
         ((skip (imm #\j))
          (joiner (star get-non-newline))) ;; extra feature
         (tuple 'join range joiner))
      (let-parses
         ((skip (imm #\~)))
         (tuple 'inspect))
      (let-parses
         ((skip (imm #\H)))
         (tuple 'help))
      (let-parses
         ((skip (imm #\x))
          (skip (imm #\newline)))
         (tuple 'context range))
      (let-parses
         ((skip (imm #\g))
          (rex get-regex-search)
          (first (make-get-command))
          (rest 
             (star
               (let-parses
                  ((skip get-global-whitespace)
                   (this (make-get-command)))
                  this)))
          (skip (get-imm #\newline)))
          (tuple 'global rex (cons first rest)))
      (let-parses
         ((skip (imm #\newline)))
         (tuple 'print
            (if (eq? range 'default)
               (tuple 'plus 'dot 1) ;; blank command = +1
               range) #false))))

;; todo: oh, this should be a kleene+ of kleene+ instead!
(define get-range
   (either
      (let-parses
         ((start get-position)
          (end
             (either
               (let-parses
                  ((delim (get-byte-if (λ (x) (or (eq? x #\,) (eq? x #\;)))))
                   (end get-position))
                  (cons delim end))
               (epsilon #false))))
         (if end
            (tuple
               (if (eq? (car end) #\,)
                  'range
                  'range-reset)
               start (cdr end))
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

(define (make-get-command)
   (let-parses
      ((skip maybe-whitespace)
       (range get-range)
       (action (get-action range make-get-command)))
      action))
   
(define usage-text
   "Usage: ed [args] [path]")

(define (add-mark env tag pos)
   (put env 'marks
      (put (get env 'marks #empty) tag pos)))

(define (get-mark env tag)
   (get (get env 'marks #empty) tag #false))

(define (match-prefix? line pat)
   (or (null? pat)
      (and (pair? line)
         (eq? (car line) (car pat))
         (match-prefix? (cdr line) (cdr pat)))))
         
(define (match-at line rex)
   (cond
      ((null? line) #false)
      ((match-prefix? line rex) #true)
      (else (match-at (cdr line) rex))))

(define (match-offset lines rex)
   (call/cc
      (λ (ret)
         (fold
            (λ (off line)
               (if (match-at line rex)
                  (ret off)
                  (+ off 1)))
            0 lines)
         #false)))
      
(define (eval-position env u d l exp default)
   (cond
      ((number? exp) exp)
      ((eq? exp 'dot) l)
      ((eq? exp 'start) 1)
      ((eq? exp 'end) (+ l (length d)))
      ((eq? exp 'default)
         (eval-position env u d l default default))
      ((tuple? exp)
         (tuple-case exp
            ((mark tag)
               (get-mark env tag))
            ((plus a b)
               (lets ((a (eval-position env u d l a default))
                      (b (eval-position env u d l b 1)))
                  (if (and a b)
                     (+ a b)
                     #false)))
            ((regex dir rex)
               (cond
                  ((null? u) ;; no current line, empty buffer
                     #false)
                  ((and (eq? dir 'up)
                     (match-offset (cdr u) rex)) =>
                     (λ (off)
                        (- l (+ off 1))))
                  ((and (eq? dir 'down)
                     (match-offset d rex)) =>
                     (λ (off)
                        (+ l (+ off 1))))
                  (else
                     #false)))
            ((minus a b)
               (lets ((a (eval-position env u d l a default))
                      (b (eval-position env u d l b 1)))
                  (if (and a b)
                     (- a b)
                     #false)))
            (else #false)))
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
            ((range-reset from to)
               (lets ((start (eval-position env u d l from   'start))
                      (end   (eval-position env u d start to 'end)))
                  (if (and start end)
                     (values start end)
                     (values #f #f))))
            ((regex dir rex)
               ;; would have succeeded in eval-position
               (values #f #f))
            (else
               (print-to stderr (str "range wat: " exp))
               (values #f #f))))
      (else
         (print-to stderr (str "range wat: " exp))
         (values #f #f))))

;; u d l n → u' d' n | #f #f #f
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
   (print 
      (if number? 
         (str l "   ")
         "") 
      (runes->string (car u)))
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

(define (join-lines u d n delim)
   (if (= n 0)
      (values u d)
      (join-lines
         (cons
            (append (car u) delim (car d))
            (cdr u))
         (cdr d)
         (- n 1)
         delim)))

;; → u' d' l' range-lines | u d l #f
(define (buff-cut-range env u d l range)
   (lets ((from to (eval-range env u d l range 'dot)))
      (if (valid-range? from to l (λ () (+ l (length d))))
         (lets
            ((u d l (seek-line u d l from))
             (this u (uncons u #f))
             (l (- l 1))
             (cutd d (split d (- to from)))
             (cutd (cons this cutd)))
            (if (null? d)
               (values u d l cutd #t)
               (values (cons (car d) u) (cdr d) (+ l 1) cutd #f)))
         (values u d l #f #f))))

;; u' d' l' | #f #f #f
(define (buff-seek-line env u d l pos default)
   (if-lets
      ((start pos (eval-range env u d l pos default))
       (up dp lp (seek-line u d l pos)))
      (begin
         (values up dp lp))
      (values #f #f #f)))

;; u d l lines → u' d' l'
(define (buff-append u d l lines)
   (values 
      (append (reverse lines) u)
      d
      (+ l (length lines))))

;; move temporarily up, allowing 0 line
(define (move-up u d l)
   (if (null? u)
      (values u d l)
      (values (cdr u) (cons (car u) d) (- l 1))))

(define (weight x)
   (write-bytes stdout
      (cond
         ((eq? x 'normal) '(27 #\[ #\m))
         ((eq? x 'bright) '(27 #\[ #\1 #\m))
         ((eq? x 'dim) '(27 #\[ #\2 #\m))
         ((eq? x 'reverse) '(27 #\[ #\7 #\m)))))

(define (pad n)
   (cond
      ((< n 10)     (str "    " n))
      ((< n 100)    (str "   " n))
      ((< n 1000)   (str "  " n))
      (else (str " " n))))

(define (render-row this)
   (λ (l)
      (if (equal? (car l) this)
         (begin
            (weight 'reverse)
            (print (str (pad this) "    " (list->string (cdr l))))
            (weight 'normal))
         (print
            (str (pad (car l))  "    "
               (list->string (cdr l)))))))

(define (show-context u d l n)   
   (lets
      ((up 
         (force-ll (lzip cons (liota l -1 0) (take u n))))
       (down 
         (force-ll (lzip cons (liota (+ l 1) +1 (+ l n)) d))))
      (for-each (render-row l)
         (append (reverse up) down))))

(define (maybe-context env u d l)
   (if (get env 'autocontext #false)
      (show-context u d l 4)))

;; dot is car of u, l is length of u
(define (ed es env u d l)
   (maybe-prompt env)
   ;(if (not (= (length u) l))
   ;  (print "BUG: position off sync"))
   (maybe-context env u d l)
   
   (lets 
      ((a es (uncons es #f))
       (last (getf env 'last))
       (env (put env 'last a)))
      ;(print-to stderr a)
      (if a
         (tuple-case a
            ((append pos data)
               (if-lets 
                  ((up dp lp (buff-seek-line env u d l pos 'dot))
                   (lines (cut data #\newline))
                   (up dp lp (buff-append up dp lp lines)))
                  (ed es 
                     (modified (put env 'undo (tuple u d l))) up dp lp)
                  (begin
                     (complain env "bad range")
                     (ed es env u d l))))
            ((change range data)
               (if-lets
                  ((up dp lp cutd fini (buff-cut-range env u d l range)))
                  (lets ((lines (cut data #\newline))
                         (env (put env 'undo (tuple u d l)))
                         (env (put env 'yank cutd))
                         (env (modified env)))
                     (cond
                        ((null? lines)
                           ;; act like delete if no data
                           (ed es env up dp lp))
                        (fini
                           (lets ((u d l (buff-append up dp lp lines)))
                              ;; end change, stay put
                              (ed es env u d l)))
                        (else
                           ;; middle paste, move up
                           (lets ((u d l (move-up up dp lp))
                                  (u d l (buff-append u d l lines)))
                              (ed es env u d l)))))
                  (begin
                     (complain env "bad range")
                     (ed es env u d l))))
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
            ((move range to)
               (if-lets
                  ((from to (eval-range env u d l range 'dot))
                   (target (eval-position env u d l to 'dot)))
                  (begin
                     (print "would move " from "-" to " → " target)
                     (ed es env u d l))
                  (begin
                     (print "wha")
                     (complain env "fishy addresses")
                     (ed es env u d l))))
            ((quit force?)
               ;; clean buffer: quit on q and Q
               ;; dirty buffer: quit on second q and Q
               (if (or force?
                     (not (modified? env))
                     (equal? last (tuple 'quit #false)))
                  0
                  (ed es env u d l)))
            ((print range number?)
               (lets ((from to (eval-range env u d l range 'dot)))
                  (if (valid-range? from to l (λ () (+ l (length d))))
                     (lets
                        ((u d l (seek-line u d l from))
                         (u d l (print-range env u d l to number?)))
                        (ed es env u d l))
                     (begin
                        (complain env "bad range")
                        (ed es env u d l)))))
            ((join range delim)
               (lets ((from to (eval-range env u d l range (tuple 'range 'dot (tuple 'plus 'dot 1)))))
                  (if (valid-range? from to l (λ () (+ l (length d))))
                     (lets
                        ((u d l (seek-line u d l from))
                         (env (put env 'undo (tuple u d l)))
                         (u d (join-lines u d (- to from) delim)))
                        (ed es (modified env) u d l))
                     (begin
                        (print-to stderr "?")
                        (ed es env u d l)))))
            ((delete range)
               (lets ((up dp lp cutd _ (buff-cut-range env u d l range)))
                  (if cutd
                     (ed es 
                        (modified 
                           (-> env
                              (put 'undo (tuple u d l))
                              (put 'yank cutd)))
                        up dp lp)
                     (begin
                        (complain env "bad range")
                        (ed es env u d l)))))
            ((prompt pt)
               (cond
                  ((not (equal? pt "")) 
                     ;; explicit prompt given
                     (ed es (put env 'prompt pt) u d l))
                  ((getf env 'prompt)
                     ;; prompter enabled, disable
                     (ed es (del env 'prompt) u d l))
                  (else
                     (ed es (put env 'prompt "*") u d l))))
            ((write range path)
               (lets
                  ((lines (append (reverse u) d)) ;; ignore range for now
                   (runes (foldr (λ (line out) (append line (cons #\newline out))) null lines))
                   (bytes (utf8-encode runes))
                   (path (if (equal? path "") (getf env 'path) path))
                   (port (maybe open-output-file path)))
                  (if (not (and port (write-bytes port bytes)))
                     (begin
                        (complain env "something failed")
                        (ed es env u d l))
                     (begin
                        (output env (length bytes))
                        (ed es
                           (not-modified (put env 'path path))
                           u d l)))))
            ((edit path force?)
               (if (and (modified? env) (not force?))
                  (begin
                     (complain env "current buffer is modified")
                     (ed es env u d l))
                  (if-lets
                     ((path (if (equal? path "") (getf env 'path) path))
                      (data (file->list path))
                      (nbytes (length data))
                      (runes (utf8-decode data)) ;; mandatory for now
                      (lines (cut runes #\newline))
                      (nlines (length lines)))
                     (begin
                        (output env nbytes)
                        (ed es
                           (put env 'path path)
                           (reverse lines)
                           null
                           nlines))
                     (begin
                        (complain env "something failed")
                        (ed es env u d l)))))
            ((mark pos tag)
               (lets ((pos (eval-position env u d l pos 'dot)))
                  (if pos
                     (ed es (add-mark env tag pos) u d l)
                     (begin
                        (complain env "something failed")
                        (ed es env u d l)))))
            ((facts path)
               (lets
                   ((env (if path (put env 'path path) env))
                    (file (get env 'path "?")))
                   (print file)
                   (ed es env u d l)))
             ((inspect)
                (print "ei: line " l)
                (print "ei: env " (del env 'last))
                (print "ei: last " last)
                (ed es env u d l))
            ((context pos)
               (if-lets 
                  ((u d l (buff-seek-line env u d l pos 'dot)))
                  (begin
                     (show-context u d l 4)
                     (ed es env u d l))
                  (begin
                     (complain "bad position")
                     (ed es env u d l))))
            ((help)
               (ed es 
                  (put env 'verbose-help
                     (not (getf env 'verbose-help)))
                  u d l))
            (else
               (complain env (str "Cannot grok " a))
               (ed es env u d l))))))

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
            (foldr
               (λ (path out)
                  (cons (tuple 'edit path #true) out))
               (fd->exp-stream stdin (make-get-command) syntax-error-handler)
               args)
            dict null null 0))))

(define (main args)
   (process-arguments (cdr args) 
      command-line-rules usage-text start-ed))

main

