#include "gui.h"
#include "image.h"
#include <stdarg.h>
#ifndef MIN
#define MIN(a,b) (a < b ? a : b)
#endif
#ifndef MAX
#define MAX(a,b) (a > b ? a : b)
#endif
//defined in vndb.cpp, returns the file pointer for the current log file,
//to allow for rudimentary logging in C.
FILE* get_vndb_log_file_pointer();
const char* get_event_type(SDL_Event *evt);
static int vndb_log(const char *fmt, ...){
  va_list ap;
  va_start(ap, fmt);
  int ret = vfprintf(get_vndb_log_file_pointer(), fmt, ap);
  va_end(ap);
  return ret;
}
static int print_sdl_error(const char *str){
  if(str && *str){
    return fprintf(stderr, "%s: %s.\n", str, SDL_GetError());
  } else {
    return fprintf(stderr, "%s.\n", SDL_GetError());
  }
}
SDL_EventType jpeg_event_type = -1;
int sdl_running = 0;
//don't capitalize SDL since that would be using the naming convention
//of SDL itself, which would effectively be using their `namespace`.

void destroy_sdl_context(struct sdl_context *ctx){
  if(!ctx){ return; }
//  SDL_DestroyWindow(ctx->hack);
  SDL_DestroyWindow(ctx->window);
  SDL_DestroyRenderer(ctx->renderer);
  SDL_DestroyTexture(ctx->texture);
  //We deliberately don't destroy the semaphore, we do however
  //call sem post on it after we're done.
  SDL_sem *sem = ctx->sem;
  free(ctx);
  sdl_running = 0;
  SDL_SemPost(sem);
}
struct sdl_context* create_sdl_context(SDL_sem *sem){
  struct sdl_context *ret = NULL;
  //one time initialization code.
  if(!SDL_WasInit(SDL_INIT_VIDEO)){
    SDL_Init(SDL_INIT_VIDEO);
    atexit(SDL_Quit);
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");
    jpeg_event_type = SDL_RegisterEvents(1);
    if(jpeg_event_type == (Uint32)-1){
      fprintf(stderr, "Failed to allocate an id for SDL user event.\n");
      goto error;
    }
  }
  //Get the current resolution to use as the texture size.
  SDL_DisplayMode dm;
  if(SDL_GetDesktopDisplayMode(0, &dm) != 0){
    fprintf(stderr, "Failed to get display mode");
    goto error;
  }
  ret = (struct sdl_context*)calloc(sizeof(struct sdl_context), 1);
  if(!ret){ goto error; }
  ret->window = SDL_CreateWindow("vndb_cpp",
                                 SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                 default_window_width, default_window_height,
                                 SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN);
  if(!ret->window){ goto error; }
  //No reason not to enable vsync.
  ret->renderer = SDL_CreateRenderer(ret->window, -1, SDL_RENDERER_PRESENTVSYNC);
  if(!ret->renderer){ goto error; }
  SDL_SetRenderDrawColor(ret->renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
  //Pixel format is RGB24 not RGB888, from what I can tell RGB888 is a 32 bit
  //format that just ignores the alpha channel, whereas RGB24 is just 3 8 bit bytes.
  //i.e RGB888 = 0xff000000 & R, 0x00ff0000 & G, 0x0000ff00 & B, 0x00000000 &A
  //    RGB24  = 0xff0000 & R, 0x00ff00 & G, 0x0000ff & B
  //possibly flipped for endianess. I'm commenting about this because it was
  //difficult to figure out why things weren't working because of this.
  //Create a texture that's larger than we need since we can re-size the window,
  //but we can't resize a texture (though we could just create another one).
  ret->texture = SDL_CreateTexture(ret->renderer,
                                   SDL_PIXELFORMAT_ARGB32, SDL_TEXTUREACCESS_STREAMING,
                                   dm.w, dm.h);
  if(!ret->texture){ goto error; }
//  ret->hack = SDL_CreateWindow("hack",SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
//                               0, 0, SDL_WINDOW_HIDDEN);
//  if(!ret->hack){ goto error; }

  ret->sem = sem;
  sdl_running = 1;//make sure to set this only at the very end.
  return ret;
 error:
  destroy_sdl_context(ret);
  return NULL;
}
//find the largest value 'scale' of 'step' less than 'limit' such that
//src_width*scale <= dest_width*scale && src_height*scale <= dest_height*scale
//If no nonzero value fits these constraints then step is returned.
double compute_scale_factor(double src_width, double src_height,
                            double dest_width, double dest_height,
                            double limit, double step){
  double width_scale = dest_width / src_width;
  double height_scale = dest_height / src_height;
  double exact_scale = 
    MAX(MIN(MIN(width_scale, height_scale), limit), step);
  double scale = round(exact_scale / step) * step;
  return scale;
}

//Create an SDL_Rect to use for rendering the given src rectangle as large
//a possible within height x width and without changing its aspect ratio.
SDL_Rect create_dest_rect(SDL_Rect src, int width, int height){

  double scale = compute_scale_factor(src.w, src.h,
                                      width, height, 10.0, 0.125);
  SDL_Rect dst = src;
  dst.w *= scale;
  dst.h *= scale;
  //Make the image centered.
  dst.x = width / 2 - dst.w / 2;
  dst.y = height / 2 - dst.h / 2;
  return dst;
}
static SDL_Rect create_src_rect(struct sdl_context *ctx){
  SDL_Rect ret;
  ret.x = ret.y = 0;
  ret.w = ctx->img_width;
  ret.h = ctx->img_height;
  return ret;
}
/*
  I could probably just use a static texture & SDL_UpdateTexture, But
  I've already written the code to do things the fast way.
*/
//Draw the image currently stored in ctx->texture to the screen.
static int render_texture(struct sdl_context *ctx){
  SDL_RenderClear(ctx->renderer);
  SDL_Rect src_rect = create_src_rect(ctx);
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    print_sdl_error("Error quering renderer dimensions");
    return -1;
  }
  SDL_Rect dst_rect = create_dest_rect(src_rect, width, height);
  if(SDL_RenderCopy(ctx->renderer, ctx->texture, &src_rect, &dst_rect) != 0){
    print_sdl_error("Error copying texture to renderer");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
//Currently the jpeg is stored in the data fields of a user event, I'll
//likely change this at some point.
static int render_jpeg(struct sdl_context *ctx){
  vndb_log("Rendering jpeg.\n");
  //Clear the screen first so that we can see if we fail to render the image.
  SDL_RenderClear(ctx->renderer);
  struct decompressed_image img;
  //We're `borrowing` the jpeg data from another thread, and we use the
  //semaphore in the sdl_context argument to indicate when we're done with it.
  uint8_t *jpeg = (uint8_t*)ctx->evt.user.data1;
  size_t jpeg_sz = (uintptr_t)ctx->evt.user.data2;
  int err = decompress_jpeg(jpeg, jpeg_sz, &img, JCS_EXT_ARGB);
//  ctx->err = err;
  SDL_SemPost(ctx->sem);//Needs to be called even if we fail at decoding.
  if(err != 0){
    fprintf(stderr, "Error decoding jpeg.\n");
    return -1;
  }
  if(img.color_space != JCS_EXT_ARGB){
    fprintf(stderr, "Don't know how to handle non RGB jpeg.\n");
    return -1;
  }
  ctx->img_width = img.width;
  ctx->img_height = img.height;
  FILE* out = fopen("tmp/last.out", "w");
  if(out){
    fwrite(img.img, img.num_components, img.width * img.height, out);
    fclose(out);
  }
  ctx->img_width = img.width;
  ctx->img_height = img.height;
  SDL_Rect img_rect = create_src_rect(ctx);
  int stride;
  unsigned char *pixels;
  if(SDL_LockTexture(ctx->texture, &img_rect, (void**)&pixels, &stride) != 0){
    fprintf(stderr, "Error locking texture.\n");
    return -1;
  }
  int row_bytes = img.width * img.num_components;
  for(size_t i = 0; i < img.height; i++){
    memcpy(pixels + i*stride, img.img + (i * row_bytes), row_bytes);
  }
  SDL_UnlockTexture(ctx->texture);
  return render_texture(ctx);
}
static int render_random_colors(struct sdl_context *ctx){
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    print_sdl_error("Error quering renderer dimensions");
    return -1;
  }
  SDL_Rect dst_rect;
  dst_rect.w = width;
  dst_rect.h = height;
  dst_rect.x = 0;
  dst_rect.y = 0;
  int stride;
  unsigned char *pixels;
  if(SDL_LockTexture(ctx->texture, &dst_rect, (void**)&pixels, &stride) != 0){
    print_sdl_error("Error locking texture");
    return -1;
  }
  for(int i = 0; i < height; i++){
    for(int j = 0; j < width*3; j++){
      *(pixels + i*stride + j) = rand();
    }
  }
  SDL_UnlockTexture(ctx->texture);
  SDL_RenderClear(ctx->renderer);
  if(SDL_RenderCopy(ctx->renderer, ctx->texture, &dst_rect, &dst_rect) != 0){
    print_sdl_error("Error copying texture to renderer");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
//This doesn't really need to be a seperate function, but making it one
//make it easy to change from hiding to minimizing the window if I want.
static void do_hide_window(struct sdl_context *ctx){
  SDL_HideWindow(ctx->window);
}
static void do_show_window(struct sdl_context *ctx){
  SDL_ShowWindow(ctx->window);
  SDL_RenderClear(ctx->renderer);
}
int sdl_main_loop(void *data){
  SDL_sem *sem = (SDL_sem *)data;
  struct sdl_context* ctx =  create_sdl_context(sem);
  SDL_SemPost(sem);
  if(!ctx){
    return 0;
  }
  SDL_EventState(SDL_MOUSEMOTION, SDL_DISABLE);
  while(1){
    //  while(SDL_WaitEvent(evt)){
    //    do_vndb_log("Got SDL event : %s.\n", get_event_type(evt));
    SDL_Event *evt = &ctx->evt;
    while(SDL_WaitEvent(evt)){
      switch(evt->type){
        case SDL_QUIT:
          destroy_sdl_context(ctx);
          return 0;
        case SDL_KEYUP:
          if(evt->key.keysym.sym == SDLK_ESCAPE){
            do_hide_window(ctx);
          }
          break;
          //If the window is closed just hide it, for any other window event
          //just redraw the image.
        case SDL_WINDOWEVENT:
          if(evt->window.event == SDL_WINDOWEVENT_CLOSE){
            //When the last window is closed SDL automatically sends
            //an SDL_QUIT event, this gets rid of that.
            SDL_FlushEvent(SDL_QUIT);
            do_hide_window(ctx);
          } else {
            SDL_RenderClear(ctx->renderer);
            render_texture(ctx);
          }
          break;
        default:
          if(evt->type == jpeg_event_type){
            do_show_window(ctx);
            render_jpeg(ctx);
          }
      }
    }
  }
  destroy_sdl_context(ctx);
  return -1;
}
SDL_sem* launch_sdl_thread(){
  SDL_Thread *thrd;
  SDL_sem *sem = SDL_CreateSemaphore(0);
  if(!sem){
    return NULL;
  }
  thrd = SDL_CreateThread(sdl_main_loop, "sdl_thread", sem);
  SDL_SemWait(sem);
  if(!sdl_running){
    SDL_DestroySemaphore(sem);
    return NULL;
  }
  return sem;
}
#if 0
//Silly little test program to make sure I can do video stuff on a thread
//than I create.
int sdl_main(void *data){
  SDL_Init(SDL_INIT_VIDEO);
  SDL_Window *win;
  SDL_Renderer *renderer;
  win = SDL_CreateWindow("test",SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                         640,480, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN);
  renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_PRESENTVSYNC);
//  SDL_Delay(1000);
  SDL_ShowWindow(win);
  SDL_Event evt;
  for(int b = 0; b < 256; b+=16){
    for(int g = 255; g >= 0; g-=16){
      for(int r = 0; r < 256; r+=16){
        SDL_SetRenderDrawColor(renderer, r, g, b, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(renderer);
        SDL_RenderPresent(renderer);
        while(SDL_PollEvent(&evt)){
          if(evt.type == SDL_QUIT){
            goto end;
          }
          if(evt.type == SDL_WINDOWEVENT &&
             evt.window.event == SDL_WINDOWEVENT_CLOSE){
            goto end;
          }
        }
      }
    }
  }
 end:
  SDL_DestroyWindow(win);
  SDL_DestroyRenderer(renderer);
  return 0;
}
int main(){
  SDL_Thread *thrd;
  thrd = SDL_CreateThread(sdl_main, "thread", NULL);
  int ret;
  printf("This is the `main` thread.\n");
  SDL_WaitThread(thrd, &ret);
  return ret;
}
#endif
const char* get_event_type(SDL_Event *evt){
  switch(evt->type){
    case SDL_AUDIODEVICEADDED:
    case SDL_AUDIODEVICEREMOVED:
      return "AudioDeviceEvent";

    case SDL_CONTROLLERAXISMOTION:
      return "ControllerAxisEvent";

    case SDL_CONTROLLERBUTTONDOWN:
    case SDL_CONTROLLERBUTTONUP:
      return "ControllerButtonEvent";

    case SDL_CONTROLLERDEVICEADDED:
    case SDL_CONTROLLERDEVICEREMOVED:
    case SDL_CONTROLLERDEVICEREMAPPED:
      return "ControllerDeviceEvent";

    case SDL_DOLLARGESTURE:
    case SDL_DOLLARRECORD:
      return "DollarGestureEvent";

    case SDL_DROPFILE:
    case SDL_DROPTEXT:
    case SDL_DROPBEGIN:
    case SDL_DROPCOMPLETE:
      return "DropEvent";

    case SDL_FINGERMOTION:
    case SDL_FINGERDOWN:
    case SDL_FINGERUP:
      return "TouchFingerEvent";

    case SDL_KEYDOWN:
    case SDL_KEYUP:
      return "KeyboardEvent";

    case SDL_JOYAXISMOTION:
      return "JoyAxisEvent";

    case SDL_JOYBALLMOTION:
      return "JoyBallEvent";

    case SDL_JOYHATMOTION:
      return "JoyHatEvent";

    case SDL_JOYBUTTONDOWN:
    case SDL_JOYBUTTONUP:
      return "JoyButtonEvent";

    case SDL_JOYDEVICEADDED:
    case SDL_JOYDEVICEREMOVED:
      return "JoyDeviceEvent";

    case SDL_MOUSEMOTION:
      return "MouseMotionEvent";

    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
      return "MouseButtonEvent";

    case SDL_MOUSEWHEEL:
      return "MouseWheelEvent";

    case SDL_MULTIGESTURE:
      return "MultiGestureEvent";

    case SDL_QUIT:
      return "QuitEvent";

    case SDL_SYSWMEVENT:
      return "SysWMEvent";

    case SDL_TEXTEDITING:
      return "TextEditingEvent";

    case SDL_TEXTINPUT:
      return "TextInputEvent";

    case SDL_USEREVENT:
      return "UserEvent";

    case SDL_WINDOWEVENT:
      return "WindowEvent";
  }
}
