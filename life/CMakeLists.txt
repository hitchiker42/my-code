cmake_minimum_required(VERSION 3.8)
project(life)
add_executable(life_ncurses life.c life_ncurses.c)
add_executable(life_sdl life.c life_sdl.c
  util.c life_sdl_eventloop.c life_sdl_work.c sdl_util.c)
execute_process(COMMAND sdl2-config --libs OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE sdl_link_flags)
execute_process(COMMAND sdl2-config --cflags OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE sdl_compile_flags)
target_link_libraries(life_ncurses ncurses)
target_link_libraries(life_sdl ${sdl_link_flags})
target_compile_options(life_sdl PUBLIC ${sdl_compile_flags})
target_compile_features(life_sdl PUBLIC c_std_99)
target_compile_features(life_ncurses PUBLIC c_std_99)
