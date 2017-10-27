;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname as3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require rsound)
(require 2htdp/batch-io)
(require racket/string)
(require 2htdp/universe)
(make-pstream)
(define 1a (rs-read "/wav/I-a-133.wav"))
(define 1b (rs-read "/wav/I-a-133.wav"))
(define 2a (rs-read "/wav/II-a-133.wav"))
(define 2b (rs-read "/wav/II-a-133.wav"))
(define 3a (rs-read "/wav/II-a-133.wav"))
(define 3b (rs-read "/wav/II-a-133.wav"))
(define 4a (rs-read "/wav/II-a-133.wav"))
(define 4b (rs-read "/wav/II-a-133.wav"))

(define ps (make-pstream))

(define-struct songstate [v1a v1b v2a v2b violaa violab celloa cellob songpos])
(define allOn (make-songstate #t #t #t #t #t #t #t #t 0))


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
                     (songstate-celloa ss)(not (songstate-cellob ss))(songstate-songpos ss))]))

(big-bang allOn
          [on-tick ticker]
          [on-key keyhandler]
          [to-draw renderer]
          )