#include "gui.h"
int jpeg_event_type = -1;
int sdl_running = 0;
int my_event_filter(void *data, SDL_Event *event){
  if(event->type == SDL_QUIT || event->type == jpeg_event_type){
    return 1;
  } else {
    return 0;
  }
}
//don't capitalize SDL since that would be using the naming convention
//of SDL itself, which would effectively be using their `namespace`.

void destroy_sdl_context(sdl_context *ctx){
  if(!ctx){ return; }
  SDL_DestroyWindow(ctx->hack);
  SDL_DestroyWindow(ctx->window);
  SDL_DestroyRenderer(ctx->rendeder);
  SDL_DestroyTexture(ctx->texture);
  //We deliberately don't destroy the semaphore, 
  //since it's `owned` by another thread.
  free(ctx);
  sdl_running = 0;
}
sdl_context* create_sdl_context(SDL_Semaphore *sem){
  sdl_context *ret = NULL;
  //one time initialization code.
  if(!SDL_WasInit(SDL_INIT_VIDEO)){
    SDL_Init(SDL_INIT_VIDEO);
    atexit(SDL_Quit);
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");
    jpeg_event_type = SDL_RegisterEvents(1);
  }
  //Get the current resolution to use as the texture size.
  SDL_DisplayMode dm;
  if(SDL_GetDesktopDisplayMode(0, &dm) != 0){
    fprintf(stderr, "Failed to get display mode");
    goto error;
  }
  ret = (sdl_context*)calloc(sizeof(sdl_context), 1);
  if(!ret){ goto error; }
  ret->hack = SDL_CreateWindow("hack",SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                               0, 0, SDL_WINDOW_HIDDEN);
  if(!ret->hack){ goto error; }
  ret->window = SDL_CreateWindow("vndb_cpp",
                                 SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                 default_window_width, default_window_height,
                                 SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIDDEN);
  if(!ret->window){ goto error; }
  //No reason not to enable vsync.
  ret->renderer = SDL_CreateRenderer(ret->window, -1, SDL_RENDERER_PRESENTVSYNC);
  if(!ret->renderer){ goto error; }
  //Create a texture that's larger than we need since we can re-size the window,
  //but we can't resize a texture (though we could just create another one).
  ret->texture = SDL_CreateTexture(ret->renderer,
                                   SDL_PIXELFORMAT_RGB888, SDL_TEXTUREACCESS_STREAMING,
                                   dm->width, dm->height);
  if(!ret->texture){ goto error; }
  sdl_running = 1;//make sure to set this only at the very end.
  return ret;
 error:
  destroy_sdl_context(ret);
  return NULL;
}
//Create an SDL_Rect to use for rendering the given src rectangle as large
//a possible within height x width and without changing its aspect ratio.
SDL_Rect create_dest_rect(SDL_Rect src, int width, int height){
  int scale = 1;
  //limit to 8x, just to prevent issues with really small images
  //or really just as a sanity check to avoid an infinite loop.
  while((scale <= 8) &&
        ((src.w * scale) < width)
        ((src.h * scale) < height)){
    scale++;
  }
  SDL_Rect dst = src;
  dst.w *= scale;
  dst.h *= scale;
  //Make the image centered.
  dst.x = width / 2 - dst.w / 2;
  dst.y = height / 2 - dst.h / 2;
  return dst;
}
SDL_Rect create_src_rect(sdl_context *ctx){
  SDL_Rect ret;
  ret.x = ret.y = 0;
  ret.width = ctx->img_width;
  ret.height = ctx->img_height;
  return ret;
}
/*
  I could probably just use a static texture & SDL_UpdateTexture, But
  I've already written the code to do things the fast way.
*/
//Draw the image currently stored in ctx->texture to the screen.
int render_texture(sdl_context *ctx){
  SDL_RenderClear(ctx->rendeder);
  SDL_Rect src_rect = create_src_rect(ctx);
  int width, height;
  if(SDL_GetRendererOutputSize(ctx->renderer, &width, &height) != 0){
    fprintf(stderr, "Error quering renderer dimensions.\n");
    return -1;
  }
  SDL_Rect dst_rect = create_dest_rect(src_rect, width, height);
  if(SDL_RenderCopy(ctx->renderer, ctx->texture, &src_rect, &dst_rect) != 0){
    fprintf(stderr, "Error copying texture to renderer.\n");
    return -1;
  }
  SDL_RenderPresent(ctx->renderer);
  return 0;
}
//Currently the jpeg is stored in the data fields of a user event, I'll
//likely change this at some point.
int render_jpeg(sdl_context *ctx){
  //Clear the screen first so that we can see if we fail to render the image.
  SDL_RenderClear(ctx->renderer);

  struct decompressed_image img;
  //We're `borrowing` the jpeg data from another thread, and we use the
  //semaphore in the sdl_context argument to indicate when we're done with it.
  uint8_t *jpeg = ctx->evt.user.data1;
  size_t jpeg_sz = (uintptr_t)ctx->evt.user.data2;
  int err = decompress_jpeg(jpeg, jpeg_sz, &img);
//  ctx->err = err;
  SDL_SemPost(ctx->sem);//Needs to be called even if we fail at decoding.
  if(err != 0){
    fprintf(stderr, "Error decoding jpeg.\n");
    return -1;
  }
  if(img.color_space != JCS_RGB){
    fprintf(stderr, "Don't know how to handle non RGB jpeg.\n");
    return -1;
  }
  ctx->img_width = img.width;
  ctx->img_height = img.height;
  SDL_Rect img_rect = create_src_rect(ctx);
  int stride;
  void *pixels;
  if(SDL_LockTexture(ctx->texture, &img_rect, &pixels, &stride) != 0){
    fprintf(stderr, "Error locking texture.\n");
    return -1;
  }
  int row_bytes = img.width * img.num_components;
  for(int i = 0; i < img.height; i++){
    memcpy(pixels + i*stride, img.img + (i * row_bytes), row_bytes);
  }
  SDL_UnlockTexure(ctx->texture);
  return render_texture(ctx);
}
//This doesn't really need to be a seperate function, but making it one
//make it easy to change from hiding to minimizing the window if I want.
int do_hide_window(sdl_context *ctx){
  SDL_HideWindow(ctx->window);
  return 0;
}
//Called when requested to display an image, exitied when window closed.
int active_event_loop(sdl_context *ctx){
  SDL_Event *evt = &ctx->evt;
  //We don't need to redraw on every frame so use WaitEvent rather than 
  //PollEvent to allow the thread to sleep if possible.
  while(SDL_WaitEvent(evt)){
    switch(evt->type){
      case SDL_QUIT:
        return 1;
      case SDL_KEYUP:
        if(evt->keysym.sym == SDL_SCANCODE_ESCAPE){
          return do_hide_window(ctx);
        }
      case SDL_USEREVENT:
        render_jpeg(ctx);
        break;
      //If the window is closed just hide it, for any other window event
      //just redraw the image.
      case SDL_WINDOWEVENT:
        if(evt->window.event == SDL_WINDOWEVENT_CLOSE){
          return do_hide_window(ctx);
        } else {
          render_texture(ctx);
        }
        break;
      default:
        //do nothing
    }
    if(ctx->evt.type == SDL_WINDOWEVENT){
    } else if(ctx->evt.type == SDL_USEREVENT){
    } else if(ctx->evt.type == SDL_QUIT){
      return 1;
    }
  }
  fprintf(stderr, "Error in SDL_WaitEvent: %s.\n", SDL_GetError());
  return 1;
}
int sdl_main_loop(void *data){
  SDL_Semaphore *sem = data;
  sdl_context *ctx = create_sdl_context(sem);  
  //The thread waiting on the semaphore uses a timed wait with a decently
  //long timeout, we use that timeout as an indication that we got an error
  //here, so it's important to not call sem_post if we fail.
  if(!ctx){
    SDL_SemPost(ctx->sem);
    return -1;
  }

  SDL_SemPost(ctx->sem);
  //We don't care about mouse motion and ignoring it should
  //help avoid waking up the thread unnecessarily
  SDL_EventState(SDL_MOUSEMOTION, SDL_DISABLE);
  int quit = 0;
  while(1){
    SDL_SetEventFilter(my_event_filter, NULL);
    SDL_WaitEvent(&ctx->evt);
    if(ctx->evt.type == SDL_QUIT){
      goto end;
    }
    if(ctx->evt.type == jpeg_event_type){
      render_jpeg(ctx);
      //This may cause a crash I'm not sure, there doesn't appear to be
      //a documented way to remove an existing event filter.
      SDL_SetEventFilter(NULL, NULL);
      if(active_event_loop(ctx) != 0){
        goto end;
      }      
    }
  }
 end:
  destroy_sdl_context(ctx);
}
SDL_Semaphore* launch_sdl_thread(){
  SDL_Thread *thrd;
  SDL_Semaphore *sem = SDL_CreateSemaphore(0);
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
