;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 3-05|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;
;;   Part 1   ;;
;;Drawing the ;;      
;;World State ;;
;;;;;;;;;;;;;;;;


;a World State is (make-ws 1a 1b 2a 2b 3a 3b 4a 4b SFX)
;where each entry is a track/sound
;represented by a boolean
;where #t is the song playing
;and #f is the song 'muted'

(define-struct ws [1a 1b 2a 2b 3a 3b 4a 4b SFX])

(define initial-state
  (make-ws #t #t #t #t #t #t #t #t #t))

;Drawn representation of part "A" tracks
(define A (circle 10 "solid" "black"))
(define A-light (circle 10 "solid" "red")) ;;A tracks when playing

;Drawn representation of part "B" tracks
(define B (square 20 "solid" "black"))
(define B-light (square 20 "solid" "red")) ;;B tracks when playing

;Drawn representation of 'extra/special' sounds
(define SFX (star 20 "solid" "black"))
(define SFX-light (star 20 "solid" "yellow")) ;;SFX tracks when playing

;Background
(define image-BG (rectangle 300 800 "solid" "white"))

;--------------------------------------------------------------------------------------------------


;Draws an image of the World State
;World State --> Image

;Our goal is the make the shapes 'light up'
;when their corresponding song is playing
;The layout of the images and their corresponding
;song is..
;
; Song1a   Song1b
; Song2a   Song2b
; Song3a   Song3b
; Song4a   Song4b
;       SFX

(check-expect (draw-image (make-ws #t #t #t #f #f #f #t #t #f))
              (place-image A-light 20 20
              (place-image B-light 60 20
              (place-image A-light 20 50
              (place-image B 60 50
              (place-image A 20 80
              (place-image B 60 80
              (place-image A-light 20 110
              (place-image B-light 60 110
              (place-image SFX 40 110
              image-BG
              ))))))))))

(check-expect (draw-image (make-ws #f #f #f #t #t #t #f #f #t))
              (place-image A 20 20
              (place-image B 60 20
              (place-image A 20 50
              (place-image B-light 60 50
              (place-image A-light 20 80
              (place-image B-light 60 80
              (place-image A 20 110
              (place-image B 60 110
              (place-image SFX-light 40 110
              image-BG
              ))))))))))

(define (draw-image ws)
  (place-image (cond
                 [(ws-1a ws) A-light] ;if state is true (song is playing), shape is red
                 [(not (ws-1a ws)) A]) ;if state is false (song muted), shape is black
               20 20
  (place-image   (cond
                 [(ws-1b ws) B-light]
                 [(not (ws-1b ws)) B])
               60 20
  (place-image  (cond
                 [(ws-2a ws) A-light]
                 [(not (ws-2a ws)) A])
               20 50
  (place-image  (cond
                 [(ws-2b ws) B-light]
                 [(not (ws-2b ws)) B])
               60 50
  (place-image  (cond
                 [(ws-3a ws) A-light]
                 [(not (ws-3a ws)) A])
               20 80
  (place-image  (cond
                 [(ws-3b ws) B-light]
                 [(not (ws-3b ws)) B])
               60 80
  (place-image  (cond
                 [(ws-4a ws) A-light]
                 [(not (ws-4a ws)) A])
               20 110
  (place-image  (cond
                 [(ws-4b ws) B-light]
                 [(not (ws-4b ws)) B])
               60 110
  (place-image  (cond
                 [(ws-SFX ws) SFX-light]
                 [(not (ws-SFX ws)) SFX])
               40 110
                 image-BG
               ))))))))))

;--------------------------------------------------------------------------------------------------

;handles an on-key event
;from the keys 1-9
;World State event --> World State



;this part is so ugly because I had to maintain
;the previous state before the on-key event, yet
;only change the state of the relevent part.
;To maintain the previous state, I had to do (ws-name ws)
;because then that output for the World State
;would be the same as the previous World State's
;...which led to this mess


(define (key-handlerer ws key)
   (cond
    [(and (ws-1a ws) (key=? key "1")) ;checks to see whether ws-1a is currently #t (red shape) and turns it #f (black)
             (make-ws #f (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-1a ws)) (key=? key "1"))
             (make-ws #t (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-1b ws) (key=? key "2"))
             (make-ws (ws-1a ws) #f (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-1b ws)) (key=? key "2"))
             (make-ws (ws-1a ws) #t (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-2a ws) (key=? key "3"))
             (make-ws (ws-1a ws) (ws-1b ws) #f (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-2a ws)) (key=? key "3"))
             (make-ws (ws-1a ws) (ws-1b ws) #t (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-2b ws) (key=? key "4"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) #f (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-2b ws)) (key=? key "4"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) #t (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-3a ws) (key=? key "5"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) #f (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-3a ws)) (key=? key "5"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) #t (ws-3b ws) (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-3b ws) (key=? key "6"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) #f (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-3b ws)) (key=? key "6"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) #t (ws-4a ws) (ws-4b ws) (ws-SFX ws))]
    [(and (ws-4a ws) (key=? key "7"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) #f (ws-4b ws) (ws-SFX ws))]
    [(and (not (ws-4a ws)) (key=? key "7"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) #t (ws-4b ws) (ws-SFX ws))]
    [(and (ws-4b ws) (key=? key "8"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) #f (ws-SFX ws))]
    [(and (not (ws-4b ws)) (key=? key "8"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) #t (ws-SFX ws))]
    [(and (ws-SFX ws) (key=? key "9"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) #f)]
    [(and (not (ws-SFX ws)) (key=? key "9"))
             (make-ws (ws-1a ws) (ws-1b ws) (ws-2a ws) (ws-2b ws) (ws-3a ws) (ws-3b ws) (ws-4a ws) (ws-4b ws) #t)]
    [else ws]
    ))

(define all-mute
  (make-ws #f #f #f #f #f #f #f #f #f))
(check-expect (key-handlerer initial-state "1")
              (make-ws #f #t #t #t #t #t #t #t #t))
(check-expect (key-handlerer all-mute "2")
              (make-ws #f #t #f #f #f #f #f #f #f))
(check-expect (key-handlerer initial-state "3")
              (make-ws #t #t #f #t #t #t #t #t #t))
(check-expect (key-handlerer initial-state "4")
              (make-ws #t #t #t #f #t #t #t #t #t))
(check-expect (key-handlerer initial-state "5")
              (make-ws #t #t #t #t #f #t #t #t #t))
(check-expect (key-handlerer initial-state "6")
              (make-ws #t #t #t #t #t #f #t #t #t))
(check-expect (key-handlerer initial-state "7")
              (make-ws #t #t #t #t #t #t #f #t #t))
(check-expect (key-handlerer initial-state "8")
              (make-ws #t #t #t #t #t #t #t #f #t))
(check-expect (key-handlerer initial-state "9")
              (make-ws #t #t #t #t #t #t #t #t #f))


(define (test ws)
  (big-bang ws
   [on-key key-handlerer]
   [to-draw draw-image]
   ))

;(test initial-state)