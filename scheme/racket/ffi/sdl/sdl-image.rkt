#lang racket

; this module provides bindings for the SDL image library
(provide
 ; load an image as a texture of a given renderer
 image-load)

; -----------
; implementation

(require ffi/unsafe
         ffi/unsafe/define
         "structs.rkt")

(define (sdl-get-lib)
  (let ([type (system-type 'os)])
    (case type
      [(unix)     "libSDL2_image"]
      [(windows)  "SDL2_image"]
      [(macosx)   "libSDL2_image"]
      [else (error "Platform not supported: " type)])))

(define-ffi-definer define-sdl-img (ffi-lib (sdl-get-lib) #f))

(define-sdl-img IMG_Load (_fun _path -> _SDL_Surface-pointer))
(define-sdl-img IMG_LoadTexture
  (_fun _SDL_Renderer _path -> _SDL_Texture))

(define (image-load renderer path)
  (IMG_LoadTexture renderer (path->complete-path path)))
