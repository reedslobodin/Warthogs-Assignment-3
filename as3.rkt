;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname as3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require racket/string)
(require 2htdp/universe)
(require rsound)

(define Ia (rs-read "wav/I-a-133.wav"))
(define IIa (rs-read "wav/II-a-133.wav"))

(define IIIa (resample .5 (rs-read "wav/III-a-133.wav")))

(define IVa(rs-read "wav/IV-a-133.wav"))
(define Ib (rs-read "wav/I-b-133.wav"))
(define IIb (rs-read "wav/II-b-133.wav"))
(define IIIb (rs-read "wav/III-b-133.wav"))
(define IVb (rs-read "wav/IV-b-133.wav"))

;;make a pstream for every part
(define 1a (make-pstream))
(define 1b (make-pstream))
(define 2a (make-pstream))
(define 2b (make-pstream))
(define 3a (make-pstream))
(define 3b (make-pstream))
(define 4a (make-pstream))
(define 4b (make-pstream))
;;set default volumes for each stream to 1/8
;;to eliminate crackling
(pstream-set-volume! 1a 1/8)
(pstream-set-volume! 1b 1/8)
(pstream-set-volume! 2a 1/8)
(pstream-set-volume! 2b 1/8)
(pstream-set-volume! 3a 1/8)
(pstream-set-volume! 3b 1/8)
(pstream-set-volume! 4a 1/8)
(pstream-set-volume! 4b 1/8)


;;cheesy bit of code to do two things with one function
;;taken from sleep-dj
(define (both a b) b)

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

;;sets volumes for the pstreams for specific parts
(define (set-volume ss part)
(cond
  [(= part 1)
  (cond [(songstate-v1a ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])]
  [(= part 2)
  (cond [(songstate-v1b ss)(pstream-set-volume! 1b 1/8)]
        [else (pstream-set-volume! 1b 0)])]
  [(= part 3)
  (cond [(songstate-v2a ss)(pstream-set-volume! 2a 1/8)]
        [else (pstream-set-volume! 2a 0)])]
  [(= part 4)
  (cond [(songstate-v2b ss)(pstream-set-volume! 2b 1/8)]
        [else (pstream-set-volume! 2b 0)])]
  [(= part 5)
  (cond [(songstate-violaa ss)(pstream-set-volume! 3a 1/8)]
        [else (pstream-set-volume! 3a 0)])]
  [(= part 6)
  (cond [(songstate-violab ss)(pstream-set-volume! 3b 1/8)]
        [else (pstream-set-volume! 3b 0)])]
  [(= part 7)
  (cond [(songstate-celloa ss)(pstream-set-volume! 4a 1/8)]
        [else (pstream-set-volume! 4a 0)])]
  [(= part 8)
  (cond [(songstate-cellob ss)(pstream-set-volume! 4b 1/8)]
        [else (pstream-set-volume! 4b 0)])]))


;set the volumes of all the pstreams for all the parts to what they should be
;based on their states
(define (volumesetter ss)
  (both (set-volume ss 1)
  (both (set-volume ss 2)
  (both (set-volume ss 3)
  (both (set-volume ss 4)
  (both (set-volume ss 5)
  (both (set-volume ss 6)
  (both (set-volume ss 7)
   (set-volume ss 8)))))))))
                    
;; taken from sleepy-dj                   
;; is it time to play the next chunk, yet?
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))
;;taken from sleepy-dj
;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;;taken from sleepy-dj
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FRAME-RATE))
;;taken from sleepy-dj
;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;;taken from sleepy-dj
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))
;determines whether to queue a new section if too close to the end
;frame --> boolean
(define (end-frames song-frame)
  (cond
    [(< (+ song-frame PLAY-FRAMES) (rs-frames Ia)) (+ song-frame PLAY-FRAMES)]
    [else (- (rs-frames Ia) 1)]))
;; taken from sleepy-dj and then modified to queue 8 pstreams
;; queue up the next fragment
(define (queue-next-fragment song-frame frame-to-play val)
  (cond
    [(< song-frame (- (rs-frames Ia) PLAY-FRAMES))
  (andqueue 1a (clip Ia song-frame (end-frames song-frame))frame-to-play
            (andqueue 1b (clip Ib song-frame (end-frames song-frame))frame-to-play
                      (andqueue 2a (clip IIa song-frame (end-frames song-frame))frame-to-play
                                (andqueue 2b (clip IIb song-frame (end-frames song-frame))frame-to-play
                                          (andqueue 3a (clip IIIa song-frame (end-frames song-frame))frame-to-play
                                                    (andqueue 3b (clip IIIb song-frame (end-frames song-frame))frame-to-play
                                                              (andqueue 4a (clip IVa song-frame (end-frames song-frame))frame-to-play
                                                                        (andqueue 4b (clip IVb song-frame (end-frames song-frame))frame-to-play val))))))))]
    [else val]
  ))
;;songstate
;;boolean for each part determining whether it is played or not
;;a songposition in frames
(define-struct songstate [v1a v1b v2a v2b violaa violab celloa cellob songpos])
;;start with all parts on at frame 0
(define allOn (make-songstate #t #t #t #t #t #t #t #t 0))

(define (draw-image ws)
  (place-image (cond
                 [(songstate-v1a ws) A-light] ;if state is true (song is playing), shape is red
                 [(not (songstate-v1a ws)) A]) ;if state is false (song muted), shape is black
               20 20
  (place-image   (cond
                 [(songstate-v1b ws) B-light]
                 [(not (songstate-v1b ws)) B])
               60 20
  (place-image  (cond
                 [(songstate-v2a ws) A-light]
                 [(not (songstate-v2a ws)) A])
               20 50
  (place-image  (cond
                 [(songstate-v2b ws) B-light]
                 [(not (songstate-v2b ws)) B])
               60 50
  (place-image  (cond
                 [(songstate-violaa ws) A-light]
                 [(not (songstate-violaa ws)) A])
               20 80
  (place-image  (cond
                 [(songstate-violab ws) B-light]
                 [(not (songstate-violab ws)) B])
               60 80
  (place-image  (cond
                 [(songstate-celloa ws) A-light]
                 [(not (songstate-celloa ws)) A])
               20 110
  (place-image  (cond
                 [(songstate-cellob ws) B-light]
                 [(not (songstate-cellob ws)) B])
               60 110 image-BG
  )))))))))

(define (ticker ss)
  (cond [(time-to-play? (songstate-songpos ss) (pstream-current-frame 1a))
  (queue-next-fragment (songstate-songpos ss) (+ (songstate-songpos ss) PLAY-FRAMES) (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss) (+ (songstate-songpos ss) PLAY-FRAMES)))]
        [else ss]
    ))   
;handles an on-key event
;from the keys 1-9
;World State event --> World State
(define (keyhandler ss key)
  ;;set volumes for the streams and handle key presses
  (both (volumesetter ss)(cond
    [(key=? key "1")
     (make-songstate (not (songstate-v1a ss))(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss) (songstate-songpos ss))]
    [(key=? key "2")
     (make-songstate (songstate-v1a ss) (not (songstate-v1b ss))
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "3")
     (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (not (songstate-v2a ss))(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "4")
     (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(not (songstate-v2b ss))(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "5")
     (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(not (songstate-violaa ss))(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "6")
     (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(not (songstate-violab ss))
                     (songstate-celloa ss)(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "7")
     (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (not (songstate-celloa ss))(songstate-cellob ss)(songstate-songpos ss))]
    [(key=? key "8")
    (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(not (songstate-cellob ss))(songstate-songpos ss)) ]
    [(key=? key "a")
    (make-songstate #t #f #f #f #f #f #f #f(songstate-songpos ss)) ]
    [(key=? key "s")
    (make-songstate #f #t #f #f #f #f #f #f(songstate-songpos ss)) ]
    [(key=? key "d")
    (make-songstate #f #f #t #f #f #f #f #f(songstate-songpos ss)) ]
    [(key=? key "f")
    (make-songstate #f #f #f #t #f #f #f #f(songstate-songpos ss)) ]
    [(key=? key "g")
    (make-songstate #f #f #f #f #t #f #f #f(songstate-songpos ss)) ]
    [(key=? key "h")
    (make-songstate #f #f #f #f #f #t #f #f(songstate-songpos ss)) ]
    [(key=? key "j")
    (make-songstate #f #f #f #f #f #f #t #f(songstate-songpos ss)) ]
    [(key=? key "k")
    (make-songstate #f #f #f #f #f #f #f #t(songstate-songpos ss)) ]
    
    [else ss])))
;;all streams muted
(define all-mute
  (make-songstate #f #f #f #f #f #f #f #f 0))
;;testing keyhandler
(check-expect (keyhandler allOn "1")
              (make-songstate #f #t #t #t #t #t #t #t 0))
(check-expect (keyhandler all-mute "2")
              (make-songstate #f #t #f #f #f #f #f #f 0))
(check-expect (keyhandler allOn "3")
              (make-songstate #t #t #f #t #t #t #t #t 0))
(check-expect (keyhandler allOn "4")
              (make-songstate #t #t #t #f #t #t #t #t 0))
(check-expect (keyhandler allOn "5")
              (make-songstate #t #t #t #t #f #t #t #t 0))
(check-expect (keyhandler allOn "6")
              (make-songstate #t #t #t #t #t #f #t #t 0))
(check-expect (keyhandler allOn "7")
              (make-songstate #t #t #t #t #t #t #f #t 0))
(check-expect (keyhandler allOn "8")
              (make-songstate #t #t #t #t #t #t #t #f 0))
(check-expect (keyhandler allOn "9")
              (make-songstate #t #t #t #t #t #t #t #t 0))

(big-bang allOn
          [on-tick ticker TICK-LEN]
          [on-key keyhandler]
          [to-draw draw-image]
          )

; ASSIGNMENT 3 PARAGRAPH WRITE UP

; Our process was actually quite simple. We first would define each piece of music as an rsound. Then
; we made a pstream for each set of sounds 1-4 A&B. We then would make sure the volume was low, 1/8 scale
; for each, so the volume wouldn't cause the sound wave to relapse and cause issues. We then made a structure
; songstate that would contain 8 booleans, 1 for each file, and the last being a number representing the song
; position. Then we used a 2*4 grid of triangles to show when each song is playing based on the color of the
; triangle and the keyhandler inputs the pressing of the keys 1-8 to turn on and off the different parts
; of the song 1-4 A&B. We then would use the ticker inorder to manipulate the pstream and makesure the
; song postions lined up with eachother, and would then make songstates for each part of the quarter, A&B.
; And if the song postion doesn't change then it just returns the same songstate ss. Then we created the
; time-to-play? fuction which determines whether it is time to play the next chunk yet or not?
; We then defined the ticks to be 1/40, with a max lead time for queueing the next sound at 3/80 * whatever
; our framerate is. We then defined each segment to be 1/20 of a second long, and then also made a function to
; determine the frames as well. Our next piee of code would then queue up the next fragment up so the pstream
; in order to make it smoother than a big-bang sound play. We then added the big-bang to bring it all together.
; We would end our code with a volume setter in order to make sure the volume went from 1/8 to 0 when keys were
; pressed for each part 1-8. We hope you enjoy our Assignment 3!
