#lang racket

(require racket/tcp)
(require bencode-codec)
(require (planet williams/science/random-source))


(define (create-listener)
  (tcp-listen (+ 14000 (random-integer 45000))  [4 #t]))

#;(+ 14000 (random-indeger 45000))
