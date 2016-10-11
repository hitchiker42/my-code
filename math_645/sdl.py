import sys, sdl2, sdl2.ext
from sdl2 import *
from functools import reduce
import operator as op
sdl2.ext.init()
sdl2.ext.Window("test",(800,800), flags=SDL_WINDOW_SHOWN|SDL_WINDOW_RESIZABLE)
