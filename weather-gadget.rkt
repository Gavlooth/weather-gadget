#||#

#lang racket

(require racket/date)
(require srfi/26)
(require xml)
(require sxml/sxpath)
(require simple-http)
(require racket/gui/base)
(require threading)
(require net/url)
(require html-parsing)
(require describe)

(define penteli (string->url   "http://penteli.meteo.gr/stations/kos"))

(define (get-weather-data)
  (with-handlers
    ([exn:fail? (lambda (exn) (displayln "\nCannot get weather data!"))])
    (let* ([converter  (bytes-open-converter "cp1253" "UTF-8")]
           [in (get-pure-port penteli #:redirections 5)]
           [response (call-with-values
                       (lambda () (bytes-convert converter (port->bytes in)))
                       (lambda (a _ __ ) a))])
      (close-input-port in)
      (bytes-close-converter converter)
      (html->xexp (bytes->string/utf-8 response)))))


(define (weave xs ys)
  (match (list xs ys)
    [(list (cons x xs) (cons y ys)) (cons x (cons y (weave xs ys)))]
    [(list '() ys)                  ys]
    [(list xs '())                  xs]))

(define the-brush
  (new brush%
       [gradient
         (new linear-gradient%
              [x0 0]
              [y0 200]
              [x1 200]
              [y1 0]
              [stops
                (list (list 0   (make-object color% "yellow"))
                      (list 0.5 (make-object color% 0 255 0))
                      (list 1   (make-object color% 51 255 246)))])]))



(define querry-values (sxpath "//div[contains(@class, 'content_values')]/div[contains(@class, 'col left_col')]/div[contains(@class, 'col_sub dist boxshadow')]/div[contains(@class, 'line')]/div[contains(@class, 'lright')]/text()"))
(define querry-labels (sxpath "//div[contains(@class, 'content_values')]/div[contains(@class, 'col left_col')]/div[contains(@class, 'col_sub dist boxshadow')]/div[contains(@class, 'line')]/div[contains(@class, 'lleft')]/text()"))

(define weather-information-labels
  '("Temperature" "Humidity" "Dewpoint" "Wind" "Barometer" "Today's Rain"
    "Rain Rate" "Storm Total" "Monthly Rain" "Yearly Rain" "Wind Chill"
    "Heat Index" "Solar Radiation" "UV Index" "Sunrise" "Sunset"))

(define frame (new frame% [label "Weather on KOS"]
                          [width 350]
                          [height 600]))

(define msg (new message% [parent frame]
                          [label "A simple weather widget"]))

(define weather-yellow (make-color 254 235 135 0.4))


; (date-minute (seconds->date (current-seconds)))
; (date-hour (seconds->date (current-seconds)))
; (date-second (seconds->date (current-seconds)))
;


(define (spawn-rows canvas dc card)
 (letrec ([spawner  (λ (dc counter weather-values)
                      (cond
                        [(= -1 counter)
                         (letrec ([dat   (seconds->date (current-seconds))]
                                  [hour   (date-hour dat)]
                                  [minute (date-minute dat)]
                                  [sec    (date-second dat)])
                          (send dc draw-rectangle  0 0 150 30)
                          (send dc draw-rectangle  150 0 350 30)
                          (send dc draw-text "Last update" 10  10)
                          (send dc draw-text (format "~a:~a:~a" hour minute sec) 160 10))]
                        [(>= counter 0)
                         (letrec (
                                  [current-cell (- card  counter)]
                                  [offset (* 30 (+ 1 current-cell))]
                                  [weather-label (list-ref  weather-information-labels current-cell)]
                                  [weather-value (list-ref  weather-values current-cell)])
                           ; (send dc set-pen "black" 1 'solid );'transparent
                           ;
                           (send dc draw-ellipse 5 5 20 20)
                           (send dc draw-rectangle  0 offset  150 30)
                           (send dc draw-rectangle  150 offset  350 30)
                           (send dc draw-text weather-label 10 (+ offset 10))
                           (send dc draw-text weather-value 160 (+ offset 10)))
                         (spawner dc (- counter 1) weather-values)]))])
   (thread
     (λ ()
       (let rec ([weather-values (querry-values  (get-weather-data))])

         (cond
           [(void? weather-values) (void)]
           [else (spawner dc card weather-values)])
         (sleep 300)
         (send dc clear)
         (rec (querry-values (get-weather-data))))))))

(define canvas
  (new canvas% [parent frame]
              [paint-callback
               (λ (canvas dc)
                 (send dc set-pen (make-color 57 91 129 0.4)  1 'solid)
                 (send dc set-brush the-brush) ;'solid)
                 (spawn-rows canvas dc 15)
                 (send dc set-text-foreground (make-color 61 31 20)))]))

; (canvas)
(define (main-)
 (send frame show #t))

(main-)

; (send dc erase)
; (send dc set-font (make-font #:size 14 #:family 'roman
;                              #:weight 'bold))
; (send dc set-text-foreground "blue")
