#ifndef __SDL_WRAPPERS_H__
#define __SDL_WRAPPERS_H__
#include <SDL.h>
#if (defined USE_VULKAN)
#include <vulkan.h>
#endif
//Safe to include even without vulkan.h
#include <SDL_vulkan.h>

//Could make this a template to use the curiously recurring template pattern
//to enable compile time polymorphism or make the destructor virtual to
//enable runtime polymorphism.
struct sdl_window_wrapper {
  SDL_Window *win = nullptr;

  sdl_window_wrapper(int width, int height, uint32_t flags,
                     const char *title = ""){
    win = SDL_CreateWindow(title, SDL_WINDOWPOS_UNDEFINED,
                           SDL_WINDOWPOS_UNDEFINED, width, height, flags);
  }
  ~sdl_window_wrapper(){
    SDL_DestroyWindow(win);
  }
  sdl_window_wrapper(const sdl_window_wrapper& other) = delete;
  sdl_window_wrapper(const sdl_window_wrapper&& other)
    : window(other.window), renderer(other.renderer), texture(other.texture){
    other.window = other.renderer = other.texture = nullptr;
  }
  SDL_Window* window(){ return win; }
  operator bool(){ return win; }
  std::pair<int,int> window_size(){
    int w,h;
    SDL_GetWindowSize(win, &w, &h);
    return {w,h};
  }
};
/*
  I'm not crazy about doing things via inheritance, but it seems to be the best
  way, at least for now.
*/

struct sdl_window_renderer_wrapper : sdl_window_wrapper {
  SDL_Renderer *renderer = nullptr;
  //You can of course create your own textures as well.
  SDL_Texture *texture = nullptr;
  sdl_window_renderer_wrapper(int width, int height, uint32_t flags,
                              const char *title = "")
    : sdl_window_wrapper(width, height, flags, title) {
    if(!this->win){ return; } //base constructor failed
    renderer = SDL_CreateRenderer(win, -1, 0);
    texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888,
                                SDL_TEXTUREACCESS_STREAMING, width, height);
  }
  ~sdl_window_renderer_wrapper(){
    SDL_DestroyTexture(texture);
    SDL_DestroyRenderer(renderer);
    win.~sdl_window_wrapper();
  }
};
/*
  Note: this doesn't actually load opengl functions, so that still needs
  to be done, either manually using SDL_GL_GetProcAddress or, more likely,
  using an extension loading library like glew.
*/
struct sdl_window_gl_wrapper : sdl_window_wrapper {
  SDL_GLContext gl_cxt = nullptr; //SDL_GLContext is a void*
  sdl_window_gl_wrapper(int width, int height, uint32_t flags,
                            const char *title = "")
    : sdl_window_wrapper(width, height, flags | SDL_WINDOW_OPENGL, title) {
    gl_ctx = SDL_GL_CreateContext(window);
  }
  ~sdl_window_gl_wrapper(){
    SDL_GL_DeleteContext(gl_ctx);
  }
};
/**/
struct sdl_window_vulkan_wrapper : sdl_window_wrapper {
  VkInstance instance; //Defined by SDL_vulkan.h even without vulkan.h
  sdl_window_vulkan_wrapper(int width, int height, uint32_t flags,
                            const char *title = "")
    : sdl_window_wrapper(width, height, flags | SDL_WINDOW_VULKAN, title) {
    init_vk_instance();
  }
  //Returns a VkResult, but since we may not have vulkan.h we need to define
  //it as returning an int.
  int init_vk_instance(){
#ifdef VULKAN_H_
    unsigned int count;
    //Need to somehow do error checking here, or at least indicate an error

    //Get extension count
    bool err = SDL_Vulkan_GetInstanceExtensions(win, &count, nullptr);
    if(err){ return VK_ERROR_INITIALIZATION_FAILED; }
    size_t add_ext_count = 0;//allow for additional extensions
    std::vector<const char*> extensions;// = {Additonal extensions go here};
    extensions.resize(add_ext_count + count);
    err = SDL_Vulkan_GetInstanceExceptions(win, &count,
                                           extensions.data() + add_ext_count);
    if(err){ return VK_ERROR_INITIALIZATION_FAILED; }
    VkInstanceCreateInfo create_info = {};
    create_info.enabledExtensionCount = static_cast<uint32_t>(extensions.size());
    create_info.ppEnabledExtensionNames = extensions.data();

    VkResult result = vkCreateInstance(&create_info, nullptr, &instance);
    return result;
#else
    return -1;
#endif
  }
  ~sdl_window_vulkan_wrapper(){
    //Destroy VkInstance here
  }
  std::pair<int,int> drawable_size(){
    int w,h;
    SDL_Vulkan_GetDrawableSize(win, &w, &h);
    return {w,h};
  }
  bool create_surface(VkSurfaceKHR *surface){
    return SDL_Vulkan_CreateSurface(win, instance, surface);
  }
};

#endif /* __SDL_WRAPPERS_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
