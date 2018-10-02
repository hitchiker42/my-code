#ifndef __PROGRESS_BAR_H__
#define __PROGRESS_BAR_H__
#include <type_traits>
#include <stdlib.h>
#include <stdint.h>
//implementation of actual console stuff & generating / displaying the progress bar
//is done in C.
extern "C" int get_term_width();
//extern "C" double float_time();
extern "C" void get_term_info(struct progress_bar *progress_bar);
extern "C" void display_progress_bar(struct progress_bar *progress_bar);
struct progress_bar {
  const char *title;
  char *buf;
  size_t title_size;
  //number of colmuns in the window, also used as the size of buf.
  int termwidth;
  //index into buf.
  int offset = 0;
  //end is the total amount current is how much is already done.
  double current = 0.0;
  double end = 0.0;
//  double start_time;
//  double current_time;
  struct display_chars {
    char start = '[';
    char done = '=';
    char cur = '>';
    char empty = ' ';
    char end = ']';
    //Can be changed to '\n' if writing to a file instead of a terminal.
    char endline = '\r';
  } display_chars;
  int8_t finished = false;
  progress_bar(double end, const char *title = "")
    : title{title}, title_size{strlen(title)}, 
      end{std::max(end, 1.0)} {//, start_time{float_time()} {
      get_term_info(this);
      buf = (char*)malloc(termwidth + 1);
  }
  ~progress_bar(){
    finish();
    assert(buf != nullptr);
    free(buf);
    buf = nullptr;
  }
  void update(double amt){
    current += amt;
    display();
  }
  void display(){
    display_progress_bar(this);
  }
  //Display progress bar one last time and start a new line, its a bit
  //clunky but its fine for now.
  void finish(bool fill = false){
    if(!finished){
      if(fill){
        current = end;
      }
      char tmp = display_chars.endline;
      display_chars.endline = '\n';
      display_progress_bar(this);
      display_chars.endline = tmp;
      finished = true;
    }
  }
  void reset(double new_end, const char *new_title = ""){
    finish();
    finished = false;
    current = 0.0;
    end = new_end;
    //If explicitly passed a null pointer for new_title, keep the old one.
    if(new_title == nullptr){ return; }
    title = new_title;
    title_size = strlen(title);
  }
  progress_bar& operator++(){
    update(1.0);
    return *this;
  }
  progress_bar& operator+=(double amt){
    update(amt);
    return *this;
  }
};
static_assert(std::is_standard_layout_v<progress_bar>);
#endif /* __PROGRESS_BAR_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
