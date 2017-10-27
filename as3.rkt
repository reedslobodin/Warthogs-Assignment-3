;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname as3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(require racket/string)
(require 2htdp/universe)
(require rsound)

(define Ia (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/I-a-133.wav"))
(define IIa (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/II-a-133.wav"))

(define IIIa (resample .5 (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/III-a-133.wav")))

(define IVa(rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/IV-a-133.wav"))
(define Ib (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/I-b-133.wav"))
(define IIb (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/II-b-133.wav"))
(define IIIb (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/III-b-133.wav"))
(define IVb (rs-read "C:/Users/Reed/Documents/GitHub/Warthogs-Assignment-3/wav/IV-b-133.wav"))

(define ps (make-pstream))
(define 1a (make-pstream))
(define 1b (make-pstream))
(define 2a (make-pstream))
(define 2b (make-pstream))
(define 3a (make-pstream))
(define 3b (make-pstream))
(define 4a (make-pstream))
(define 4b (make-pstream))
(pstream-set-volume! 1a 1/8)
(pstream-set-volume! 1b 1/8)
(pstream-set-volume! 2a 1/8)
(pstream-set-volume! 2b 1/8)
(pstream-set-volume! 3a 1/8)
(pstream-set-volume! 3b 1/8)
(pstream-set-volume! 4a 1/8)
(pstream-set-volume! 4b 1/8)

(define (both a b) b)

(define (set-volumes ss part)
(cond
  [(= part 1)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 2)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 3)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 4)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 5)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 6)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 7)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  [(= part 8)
  (cond [(songstate-Ia ss)(pstream-set-volume! 1a 1/8)]
        [else (pstream-set-volume! 1a 0)])
  
    ;change parts to match

;; is it time to play the next chunk, yet?
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))

;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FRAME-RATE))

;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))

(define (end-frames song-frame)
  (cond
    [(< (+ song-frame PLAY-FRAMES) (rs-frames Ia)) (+ song-frame PLAY-FRAMES)]
    [else (- (rs-frames Ia) 1)]))
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

(define-struct songstate [v1a v1b v2a v2b violaa violab celloa cellob songpos])
(define allOn (make-songstate #t #t #t #t #t #t #t #t 0))

(define (renderer bool)
      (square 50 "solid" "green"))

(define (ticker ss)
  (cond [(time-to-play? (songstate-songpos ss) (pstream-current-frame ps))
  (queue-next-fragment (songstate-songpos ss) (+ (songstate-songpos ss) PLAY-FRAMES) (make-songstate (songstate-v1a ss)(songstate-v1b ss)
                     (songstate-v2a ss)(songstate-v2b ss)(songstate-violaa ss)(songstate-violab ss)
                     (songstate-celloa ss)(songstate-cellob ss) (+ (songstate-songpos ss) PLAY-FRAMES)))]
        [else ss]
    ))   

(define (keyhandler ss key)
  (cond
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
                     (songstate-celloa ss)(not (songstate-cellob ss))(songstate-songpos ss))]
    [else ss]))

(big-bang allOn
          [on-tick ticker TICK-LEN]
          [on-key keyhandler]
          [to-draw renderer]
          )