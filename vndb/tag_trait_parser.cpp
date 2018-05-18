#include "vndb.h"
//Code to parse the tag/trait dumps and create sql tables for them

//Create a unique trait name, this puts a seperator before the first
//component so it looks like a filename path, it's easy to change this
//so that the seperator is only put in between elements.
static void create_trait_name_acc(const json trait, std::string &name){
  static constexpr char seperator = '/';
/* //only put seperator between elemnets
   if(traits["parents"].empty()){ name.append(trait["name"]);
   create_trait_name_acc(trait["parent"][0], name);
   name.push_back(seperator); name.append(trait["name"]);
*/
  //If not root recur, not tail recursive in order to get the right order.
  if(!trait["parents"].empty()){
    create_trait_name_acc(trait["parent"][0], name);
  }
  name.push_back(seperator);
  name.append(trait["name"]);
}
//Construct a unique name for trait, of the form /root[/child]*
std::string create_trait_name(const json trait){
  std::string ret;
  create_trait_name_acc(trait, ret);
  return ret;
}

    
void parse_traits(const char *filename){
  FILE *f = fopen(filename, "r");
  if(!f){
    fprintf("Couldn't open %s\n", filename);
  }
  json traits_json = json::parse(f);
  fclose(f);
  auto &traits_arr = traits_json.get_ref<std::vector<json>>();
  size_t count = traits_arr.size();
  //Construct unique trait names, could just use a for loop.
  std::vector<std::string> full_names;
  full_names.reserve(count);
  std::transform(traits_arr.begin(), traits_arr.end(),
                 std::back_inserter(full_names), create_trait_name);

  //I'm assuming traits are contiguous, I could be wrong though
  for(auto &&trait : traits_arr){
    assert(trait.is_object());
    int id = trait["id"];
    if(id >= count){//This is more a check than a requirement.
      fprintf(stderr, "Error, found trait with id > number of traits\n"
              "id = %d, number of traits = %ld\n", id, count);
    }
    

      
