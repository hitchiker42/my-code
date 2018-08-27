#include <SDL2/SDL.h>
#include <unistd.h>
#include <sys/stat.h>
#include "font.h"
#include "image.h"
#include "text.h"
#include "xorshift.h"
#include "filesystem.h"
#include "hello.h"
std::unique_ptr<ft_library_wrapper> ft_lib_ptr;
std::unique_ptr<util::logger> vndb_log;
static constexpr const char *sans_font_path = "fonts/NotoSansCJK.ttc";
static constexpr int default_window_width = 640;
static constexpr int default_window_height = 480;
static constexpr int font_pt_size = 12;
const char* get_event_type(SDL_Event *evt);
struct sdl_context {
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *text_texture;
  SDL_Texture *img_texture;
  SDL_Event evt;
  ft_face_wrapper *font;
  int text_width;
  int text_height;
  int img_width;
  int img_height;
};
void destroy_sdl_context(struct sdl_context *ctx){
  if(!ctx){ return; }
  SDL_DestroyWindow(ctx->window);
  SDL_DestroyRenderer(ctx->renderer);
  SDL_DestroyTexture(ctx->text_texture);
  SDL_DestroyTexture(ctx->img_texture);
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
                                 SDL_WINDOW_RESIZABLE);
  if(!ret->window){ goto error; }
  //No reason not to enable vsync.
  ret->renderer = SDL_CreateRenderer(ret->window, -1, 
                                     SDL_RENDERER_PRESENTVSYNC);
  if(!ret->renderer){ goto error; }
  SDL_SetRenderDrawColor(ret->renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_SetRenderDrawBlendMode(ret->renderer, SDL_BLENDMODE_BLEND);
  ret->img_texture = SDL_CreateTexture(ret->renderer,
                                       SDL_PIXELFORMAT_ARGB32, 
                                       SDL_TEXTUREACCESS_STREAMING,
                                       dm.w, dm.h);
  if(!ret->img_texture){ goto error; }
  ret->font = ft_face_wrapper::init_create(*ft_lib_ptr,
                                           sans_font_path, font_pt_size);
  ret->text_texture = nullptr;
  if(!ret->font){ goto error; }
  return ret;
 error:
  destroy_sdl_context(ret);
  return NULL;
}
double compute_scale_factor(double src_width, double src_height,
                            double dest_width, double dest_height,
                            double limit = 10.0,
                            double step = 0.25){
  double width_scale = dest_width / src_width;
  double height_scale = dest_height / src_height;
  double exact_scale = 
    std::max(std::min(std::min(width_scale, height_scale), limit), step);
  double scale = round(exact_scale / step) * step;
  return scale;
}

//Create an SDL_Rect to use for rendering the given src rectangle as large
//a possible within height x width and without changing its aspect ratio.
SDL_Rect create_dest_rect(SDL_Rect src, int width, int height){
  double scale = compute_scale_factor(src.w, src.h,
                                      width, height);
  SDL_Rect dst = src;
//  dst.w *= scale;
//  dst.h *= scale;
  //Make the image centered.
  dst.x = width / 2 - dst.w / 2;
  dst.y = height / 2 - dst.h / 2;
  return dst;
}
int render_texture(struct sdl_context *ctx){
  SDL_RenderClear(ctx->renderer);
  if(!ctx->text_texture){
    SDL_RenderPresent(ctx->renderer);
    return 0;
  }
  SDL_Rect src_rect = {0,0,0,0};
  SDL_QueryTexture(ctx->text_texture, nullptr, nullptr,
                   &src_rect.w, &src_rect.h);
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    fprintf(stderr, "Error quering renderer dimensions.\n");
    return -1;
  }
  SDL_Rect dst_rect = create_dest_rect(src_rect, width, height);
//  fprintf(stderr, "Copying texture of size %dx%d to output of size %dx%d at (%d,%d).\n",
//          src_rect.w, src_rect.h, width, height, dst_rect.x, dst_rect.y);
  if(SDL_RenderCopy(ctx->renderer, ctx->text_texture, &src_rect, &dst_rect)){
    fprintf(stderr, "Error copying texture to renderer.\n");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
int save_texture_bmp(SDL_Texture *tex, std::string_view filename){
  FILE_wrapper out(filename, "w");
  if(!out){ 
    perror("fopen");
    char *tmp = get_current_dir_name();
    fprintf(stderr, "Couldn't open %s/%.*s\n",
            tmp, (int)filename.size(),filename.data());
    free(tmp);
    return -1;
  }
  int w, h;
  SDL_QueryTexture(tex, nullptr, nullptr, &w, &h);
  void *pixels;
  int stride;

  if(SDL_LockTexture(tex, nullptr, &pixels, &stride)){ 
    fprintf(stderr, "Couldn't lock texture");
    return -1; 
  }
  out.write(pixels, h * stride);
  SDL_UnlockTexture(tex);
  return 0;
}
static int render_jpeg(struct sdl_context *ctx,
                       uint8_t *jpeg, size_t jpeg_sz){
  //Clear the screen first so that we can see if we fail to render the image.
  SDL_RenderClear(ctx->renderer);
  struct decompressed_image img;
  int err = decompress_jpeg(jpeg, jpeg_sz, &img, JCS_EXT_ARGB);
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

  SDL_Rect img_rect = {0,0,0,0};
  img_rect.w = img.width;
  img_rect.h = img.height;
  int stride;
  unsigned char *pixels;
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
int event_loop(struct sdl_context *ctx,
               std::string_view text){
  SDL_EventState(SDL_MOUSEMOTION, SDL_DISABLE);
//  std::vector<std::string_view> lines = util::split(text, "\n"sv);
//  size_t line_idx = 0;
  SDL_Event *evt = &ctx->evt;
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
            if(ctx->text_texture){
              goto quit;
            }
//            std::string_view line = lines[line_idx++];
//            SDL_DestroyTexture(ctx->text_texture);
            //Swap between alpha and non alpha.
//            if(line_idx & 1){
              ctx->text_texture =
                ctx->font->render_utf8_text_argb_multiline(text, 0xff00ff,
                                                           ctx->renderer);
//            } else {
//              ctx->text_texture =
//                ctx->font->render_utf8_text_rgb(line,
//                                                0xff0000,
//                                                0x0000ff,
//                                                ctx->renderer);
//            }
            if(ctx->text_texture == nullptr){
              fprintf(stderr,"Error rendering text\n");
              exit(1);
            }
            render_texture(ctx);            
//            save_texture_bmp(ctx->text_texture, 
//                             line.substr(0, line.find_first_of(' ')));
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
    }
  }
 quit:
  destroy_sdl_context(ctx);
  return 0;
}
int main(int argc, char *const argv[]){
  struct stat buf;
  stat("font_test.log", &buf);
  if(buf.st_size > 0){
    rename("font_test.log", "font_test.log.bkup");
  }
  vndb_log = std::make_unique<util::logger>("font_test.log",
                                            util::log_level::debug);
  if(!vndb_log->out){
    fprintf(stderr, "Failed to open log file \"%s\".\n", "sdl_test.log");
  }
  ft_lib_ptr = std::make_unique<ft_library_wrapper>();
  sdl_context *ctx = create_sdl_context();
  return event_loop(ctx, hello_string_simple);
}
