#include "vndb.h"
//Code to parse the tag/trait dumps and create sql tables for them
std::string create_trait_name(const json trait){
}
void create_trait_name_acc(const json trait, std::string &name){
  if(trait["parents"].empty()){//found root trait
    name.append(trait["name"].get<std::string>());
  } else {
    //recur, non tail-recursive, since we need the parent name
    //to come before the child name.
    create_trait_name_acc(trait["parent"][0], name);
    //append 
    
void parse_traits(const char *filename){
  FILE *f = fopen(filename, "r");
  if(!f){
    fprintf("Couldn't open %s\n", filename);
  }
  json traits_json = json::parse(f);
  fclose(f);
  auto &traits_arr = traits_json.get_ref<std::vector<json>>();
  size_t count = traits_arr.size();
  //Construct unique trait names
  //Since a trait may have mulitple parents, but we only use
  //the first, it wouldn't be that hard to create a name for each parent,
  //just a bit cumbersome.
  std::vector<std::string> full_names(count);

  //I'm assuming traits are contiguous, I could be wrong though
  for(auto &&trait : traits_arr){
    assert(trait.is_object());
    int id = trait["id"];
    if(id >= count){
      fprintf(stderr, "Error, found trait with id > number of traits\n"
              "id = %d, number of traits = %ld\n", id, count);
    }
    if(trait[

      
