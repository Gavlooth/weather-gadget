#||#

#lang racket

(require xml)
(require sxml/sxpath)
(require simple-http)
(require racket/gui/base)

(require describe)

(define (weave xs ys)
  (match (list xs ys)
    [(list (cons x xs) (cons y ys)) (cons x (cons y (weave xs ys)))]
    [(list '() ys)                  ys]
    [(list xs '())                  xs]))

(define request-meteo
   (update-host  html-requester "penteli.meteo.gr"))

(define (get-weather-data)
  (get request-meteo "/stations/kos/"))

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

(define pipa '())
;(displayln pipa)
;(bytes?  (string-ref (list-ref pipa 4) 5)) 

(define (spawn-rows canvas dc card)
 (letrec ([spawner  (lambda (dc counter weather-values)
                      (cond
                        [(= 0 counter) null]
                        [(> counter 0)
                         (letrec (
                                  [current-cell (- card counter)]
                                  [offset (* 30 current-cell)]
                                  [weather-label (list-ref  weather-information-labels current-cell)]
                                  [weather-value (list-ref  weather-values current-cell)])
                           (set! pipa (cons weather-value pipa))
                           (send dc draw-rectangle  0 offset  150 30)
                           (send dc draw-rectangle  150 offset  350 30)
                           (send dc draw-text weather-label 10 (+ offset 10))
                           (send dc draw-text weather-value 160 (+ offset 10)))
                         (spawner dc (- counter 1) weather-values)]))])
   (thread (lambda ()
             (let rec ([weather-values (querry-values (html-response-body (get-weather-data)))])
               (spawner dc card weather-values)
               (sleep 300)
               (send dc clear)
               (with-handlers ([exn:fail? (lambda (exn) null)])
                 (printf "\nUpdating weather data!"))
               (rec (querry-values (html-response-body (get-weather-data)))))))))

(define canvas
  (new canvas% [parent frame]
              [paint-callback
               (lambda (canvas dc)
                 (send dc set-pen (make-color 57 91 129 0.4)  1 'solid)
                 (spawn-rows canvas dc 16)
                 (send dc set-text-foreground (make-color 61 31 20)))]))

(define (main-)
 (send frame show #t))

(main-)

