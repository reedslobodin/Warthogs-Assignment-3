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


