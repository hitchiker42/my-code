#define __GUI_H__
#include <SDL2/SDL.h>
//SDL_sem* launch_sdl_thread();
#include "vndb.h"
#include "image.h"
#include "font.h"
SDL_EventType my_event_type;
std::unique_ptr<util::logger> vndb_log;
std::unique_ptr<ft_library_wrapper> ft_lib_ptr;
static constexpr const char *sans_font_path = "data/NotoSansJP.otf";
const char* get_event_type(SDL_Event *evt);
static constexpr int default_window_width = 640;
static constexpr int default_window_height = 480;
struct sdl_context {
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *img_texture;
  SDL_Texture *saved_texture;
  SDL_Texture *text_texture;
  //store an event just to make it eaiser to break event
  //handling into seperate functions
  SDL_Event evt;
  ft_face_wrapper *font;
  //Width and height of last image drawn, we need to save these so we can
  //redraw the image if the window is moved/resized/etc..
  int img_width;
  int img_height;
};
void destroy_sdl_context(struct sdl_context *ctx){
  if(!ctx){ return; }
  SDL_DestroyWindow(ctx->window);
  SDL_DestroyRenderer(ctx->renderer);
  SDL_DestroyTexture(ctx->img_texture);
  SDL_DestroyTexture(ctx->saved_texture);
  ft_face_wrapper::destroy_free(ctx->font);
  free(ctx);
}
struct sdl_context* create_sdl_context(){
  struct sdl_context *ret = NULL;
  //one time initialization code.
  if(!SDL_WasInit(SDL_INIT_VIDEO)){
    SDL_Init(SDL_INIT_VIDEO);
    atexit(SDL_Quit);
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");
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
  //Create a texture that's larger than we need since we can re-size the window,
  //but we can't resize a texture (though we could just create another one).
  ret->img_texture = SDL_CreateTexture(ret->renderer,
                                       SDL_PIXELFORMAT_ARGB8888,
                                       SDL_TEXTUREACCESS_STREAMING,
                                       dm.w, dm.h);
  if(!ret->img_texture){ goto error; }
  ret->saved_texture = SDL_CreateTexture(ret->renderer,
                                         SDL_PIXELFORMAT_ARGB8888,
                                         SDL_TEXTUREACCESS_TARGET,
                                         dm.w, dm.h);
  if(!ret->saved_texture){ goto error; }
  ret->font = ft_face_wrapper::init_create(ft_lib_ptr,
                                           sans_font_path, 16);
  if(!ret->font){ goto error; }
  return ret;
 error:
  destroy_sdl_context(ret);
  return NULL;
}
double compute_scale_factor(int src_width, int src_height,
                            int dest_width, int dest_height,
                            double limit = 10.0,
                            double scale_step = 0.25){
  double width_scale = ((double)src_width) / dest_width;
  double height_scale = ((double)src_height) / dest_height;
  double exact_scale = std::min(std::min(width_scale, height_scale), limit);
  double scale = floor(exact_scale);
  while((scale + scale_step) < exact_scale){
    scale += exact_scale;
  }
  return scale;
}
  
//Create an SDL_Rect to use for rendering the given src rectangle as large
//a possible within height x width and without changing its aspect ratio.
SDL_Rect create_dest_rect(SDL_Rect src, int width, int height){

  double scale = compute_scale_factor(src.w, src.h,
                                      width, height);
  SDL_Rect dst = src;
  dst.w *= scale;
  dst.h *= scale;
  //Make the image centered.
  dst.x = width / 2 - dst.w / 2;
  dst.y = height / 2 - dst.h / 2;
  return dst;
}
SDL_Rect create_src_rect(struct sdl_context *ctx){
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
int render_texture(struct sdl_context *ctx){
  SDL_SetRenderTarget(ctx->renderer, nullptr);
  SDL_RenderClear(ctx->renderer);
  SDL_Rect src_rect = create_src_rect(ctx);
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    fprintf(stderr, "Error quering renderer dimensions.\n");
    return -1;
  }
  SDL_Rect dst_rect = create_dest_rect(src_rect, width, height);
  if(SDL_RenderCopy(ctx->renderer, ctx->saved_texture,
                    &src_rect, &dst_rect) != 0){
    fprintf(stderr, "Error copying texture to renderer.\n");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
//Currently the jpeg is stored in the data fields of a user event, I'll
//likely change this at some point.
int render_jpeg(struct sdl_context *ctx){
  //Clear the screen first so that we can see if we fail to render the image.
  //SDL_RenderClear(ctx->renderer);
  struct decompressed_image img;
  //We're `borrowing` the jpeg data from another thread, and we use the
  //semaphore in the sdl_context argument to indicate when we're done with it.
  uint8_t *jpeg = (uint8_t*)ctx->evt.user.data1;
  size_t jpeg_sz = (uintptr_t)ctx->evt.user.data2;
  int err = decompress_jpeg(jpeg, jpeg_sz, &img);
//  ctx->err = err;
//  SDL_SemPost(ctx->sem);//Needs to be called even if we fail at decoding.
  if(err != 0){
    fprintf(stderr, "Error decoding jpeg.\n");
    return -1;
  }
  if(img.color_space != JCS_RGB){
    fprintf(stderr, "Don't know how to handle non RGB jpeg.\n");
    return -1;
  }
  FILE_wrapper out("tmp/last.out", "w");
  out.write(img.img, img.width * img.height * img.num_components);
  out.close();
  ctx->img_width = img.width;
  ctx->img_height = img.height;
  SDL_Rect img_rect = create_src_rect(ctx);
  int stride;
  unsigned char *pixels;
  //it's weird that char* implictly converts to void* but char** has to be
  //explicitly cast to void**.
  if(SDL_LockTexture(ctx->img_texture, &img_rect, (void**)&pixels, &stride) != 0){
    fprintf(stderr, "Error locking texture.\n");
    return -1;
  }
  int row_bytes = img.width * img.num_components;
  for(size_t i = 0; i < img.height; i++){
    memcpy(pixels + i*stride, img.img + (i * row_bytes), row_bytes);
  }
  SDL_UnlockTexture(ctx->img_texture);
  return render_texture(ctx);
}
int render_random_colors(struct sdl_context *ctx){
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    fprintf(stderr, "Error quering renderer dimensions.\n");
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
    fprintf(stderr, "Error locking texture.\n");
    return -1;
  }
  for(size_t i = 0; i < height; i++){
    for(size_t j = 0; j < width*3; j++){
      *(pixels + i*stride + j) = rand();
    }
  }
  SDL_UnlockTexture(ctx->texture);
  SDL_RenderClear(ctx->renderer);
  if(SDL_RenderCopy(ctx->renderer, ctx->texture, &dst_rect, &dst_rect) != 0){
    fprintf(stderr, "Error copying texture to renderer.\n");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
int event_loop(vndb_main *vndb, struct sdl_context *ctx,
               std::vector<int> images){
  SDL_EventState(SDL_MOUSEMOTION, SDL_DISABLE);
  sqlite3_stmt_wrapper& stmt =
    vndb->get_select_by_id_stmt(vndb_main::table_type::vn_images);
  SDL_Event *evt = &ctx->evt;

  //We don't need to redraw on every frame so use WaitEvent rather than
  //PollEvent to allow the thread to sleep if possible.
  while(SDL_WaitEvent(evt)){
    switch(evt->type){
      case SDL_QUIT:
        goto quit;
      case SDL_KEYUP:
        switch(evt->key.keysym.sym){
          case SDLK_ESCAPE:
            goto quit;
          case SDLK_SPACE:
          case SDLK_RETURN:{
            if(images.empty()){
              goto quit;
            }
            int id = images.back();
            images.pop_back();
            stmt.bind(1, id);
            int res = stmt.step();
            if(res == SQLITE_DONE){
              printf("Could not find an image for vn %d.\n", id);
              //Not sure what to return here, it's not really an error but
              //it's not successful either.
              break;
            } else if(res == SQLITE_ROW){
              const void *data = stmt.get_column<const void*>(0);
              size_t data_size = stmt.get_column_bytes(0);
              printf("Got image for vn %d, writing to file tmp/%d.jpg.\n", id, id);
              char buf[256];
              snprintf(buf, 256, "tmp/%d.jpg", id);
              FILE_wrapper out(buf, "w");
              out.write(data, data_size);
              out.close();
              memset(evt, 0, sizeof(SDL_Event));
              evt->user.data1 = (void*)data;
              evt->user.data2 = (void*)data_size;
              render_jpeg(ctx);
              assert(stmt.step() == SQLITE_DONE);
              stmt.reset();
              break;
            } else {
              printf("Error executing sql.\n");
              return -1;
            }
          }
        }
        break;
      //If the window is closed just hide it, for any other window event
      //just redraw the image.
      case SDL_WINDOWEVENT:
        if(evt->window.event == SDL_WINDOWEVENT_CLOSE){
          goto quit;
        } else {
          render_texture(ctx);
        }
        break;
      default:
        if(evt->type == my_event_type){
          SDL_ShowWindow(ctx->window);
        }
    }
  }
 quit:
  destroy_sdl_context(ctx);
  return 0;
}
int main(int argc, char *const argv[]){
  if(argc < 2){
    printf("%s id ids...\n", argv[0]);
    return 0;
  }
  //rename old log file, we only do this if not logging to stderr to avoid
  //removing an old log file unnecessarily.
  rename("sdl_test.log", "sdl_test.log.bkup");
  vndb_log = std::make_unique<util::logger>("sdl_test.log",
                                            util::log_level::debug);
  if(!vndb_log->out){
    fprintf(stderr, "Failed to open log file \"%s\".\n", "sdl_test.log");
  }
  ft_lib_ptr = std::make_unique<ft_library_wrapper>();

  vndb_main vndb(default_db_file);
  if(!vndb.init_all()){
    exit(EXIT_FAILURE);
  }
  std::vector<int> ids;
  //start from the last id given since a vector is lifo not fifo.
  for(int i = argc-1; i > 0; i--){
    ids.push_back(strtol(argv[i], nullptr, 0));
  }
  struct sdl_context *ctx = create_sdl_context();
  my_event_type = (SDL_EventType)SDL_RegisterEvents(1);
  return event_loop(&vndb, ctx, ids);
}
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
