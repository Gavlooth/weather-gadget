#lang racket/base

(require racket/tcp racket/match bencode-codec)

(define port   (make-parameter 7888))
(define debug? (make-parameter #f))

(define (response-for old-msg msg)
  (let ([m (hash-set msg #"session" (hash-ref old-msg #"session" #"none"))])
    (hash-set m #"id" (hash-ref old-msg #"id" #"unknown"))))

(define (send o msg)
  (when (debug?) (printf "Sending: ~s~n" msg))
  (bencode-write msg o)
  (flush-output o))

(define (send-exception o msg ex)
  (send o (response-for msg (hash #"ex" (string->bytes/utf-8 (format "~s" ex))
                                  #"status" '(#"done")))))

(define (eval-msg o msg ns)
  (let* ([code-str (hash-ref msg #"code")]
         [code (if (equal? #"nil" code-str)
                   '0 ; work around grench quirk
                   (read (open-input-bytes code-str)))]
         [value (eval code ns)])
    (send o (response-for msg (hash #"value" (string->bytes/utf-8
                                              (format "~s" value))
                                    #"status" '(#"done"))))))

(define (register-session i o ns msg session-loop)
  (let ([id (string->bytes/utf-8 (number->string (random 4294967087) 16))])
    (send o (response-for msg (hash #"new-session" id #"status" '(#"done"))))
    (session-loop i o id ns)))

(define (session-loop i o id ns)
  (let ([msg (bencode-read i)])
    (when (debug?) (printf "Received: ~s~n" msg))
    (cond [(eof-object? msg) (when (debug?) (printf "Disconnected.~n"))]
          [(equal? #"clone" (hash-ref msg #"op"))
           (register-session i o ns msg session-loop)]
          [else (match msg
                  [(hash-table (#"op" #"eval"))
                   (with-handlers ([exn:fail? (λ (exn) (send-exception o msg exn))])
                     (eval-msg o msg ns))])
                (session-loop i o id ns)])))

(define (listen listener)
  (let-values ([(i o) (tcp-accept listener)])
    (when (debug?) (printf "Connected.~n"))
    (thread (lambda () (session-loop i o "pre-init" (make-base-namespace))))
    (listen listener)))

(module+ main
  (require racket/cmdline)

  (command-line #:program "ogion"
                #:once-each
                [("-p" "--port") port-str "Port number"
                                 (port (string->number port-str))]
                [("--debug") "Enable debug mode" (debug? #t)]
                #:args ()
                (begin
                  (printf "Starting on port ~s~n" (port))
                  (listen (tcp-listen (port) 4 #t "localhost")))))
