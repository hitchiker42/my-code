#lang racket/base
(require "main.rkt")
(require "../../util.rkt")
(define (sdl-init . flags)
  (SDL_Init (apply bitwise-ior flags)))
