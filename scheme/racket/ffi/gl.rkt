#lang racket/base
(require "../util.rkt" "../io.rkt" "ffi.rkt" "sdl/main.rkt")
(require (for-syntax racket/base racket/syntax))
(define glew-lib (ffi-lib "libGLEW"))
(define-ffi-binding glew-lib "glewInit" (_fun -> _int) "glew-init")
(define (glew-get-fun name)
  (get-ffi-obj (format #f "__~a"  name) glew-lib _fpointer))
(define (glew-get-var name)
  (get-ffi-obj (format #f "__~a" name) glew-lib _bool))
(set-ffi-obj! "glewExperimental" glew-lib _bool #t)

(define (sdl-init-gl-context w h name) 
  (SDL_Init SDL_INIT_VIDEO)
  (SDL_GL_SetAttribute 'SDL_GL_CONTEXT_MAJOR_VERSION 3)
  (SDL_GL_SetAttribute 'SDL_GL_CONTEXT_MINOR_VERSION 3)
  (SDL_GL_SetAttribute 'SDL_GL_CONTEXT_FLAGS
                       SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG)
  (SDL_GL_SetAttribute 'SDL_GL_CONTEXT_PROFILE_MASK
                       SDL_GL_CONTEXT_PROFILE_CORE)
  (SDL_GL_SetAttribute 'SDL_GL_DOUBLEBUFFER 1)
  (libc-atexit (get-ffi-obj "SDL_Quit" sdl-lib (_fun -> _void)))
  (let* ((win
         (SDL_CreateWindow name 'SDL_WINDOWPOS_CENTERED 'SDL_WINDOWPOS_CENTERED
                           w h '(SDL_WINDOW_OPENGL SDL_WINDOW_RESIZABLE)))
         (ctx (SDL_GL_CreateContext win)))
    (values win ctx)))
