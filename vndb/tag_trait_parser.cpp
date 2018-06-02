#include "vndb.h"
#include "sql.h"
#include <zlib.h>
std::unique_ptr<util::logger> vndb_log;
static constexpr std::string_view tags_uri = "/api/tags.json.gz"sv;
static constexpr std::string_view traits_uri = "/api/traits.json.gz"sv;
//Code to parse the tag/trait dumps and create sql tables for them

//Create a unique trait name, this puts a seperator before the first
//component so it looks like a filename path, it's easy to change this
//so that the seperator is only put in between elements.
static void create_trait_name_acc(const json& traits,
                                  const json& trait, std::string &name){
  static constexpr char seperator = '/';
/* //only put seperator between elemnets
   if(traits["parents"].empty()){ name.append(trait["name"]);
   create_trait_name_acc(trait["parent"][0], name);
   name.push_back(seperator); name.append(trait["name"]);
*/
  //If not root recur, not tail recursive in order to get the right order.
  if(!trait["parents"].empty()){
    //change at to normal [] if it works.
    create_trait_name_acc(traits, traits[trait["parents"][0].get<int>()], name);
  }
  name.push_back(seperator);
  name.append(trait["name"]);
}
//Construct a unique name for trait, of the form /root[/child]*
std::string create_trait_name(const json& traits, int idx){
  std::string ret;
  create_trait_name_acc(traits, traits[idx], ret);
  return ret;
}
std::string create_trait_name(const json& traits, const json& trait){
  std::string ret;
  create_trait_name_acc(traits, trait, ret);
  return ret;
}
std::vector<std::string> generate_trait_names(json& traits){
  std::vector<std::string> full_names;
  full_names.reserve(traits.size());
  DEBUG_PRINTF("Creating full trait names\n");
  for(auto &&trait : traits){
    if(!trait.is_null()){
//      DEBUG_PRINTF("On trait %d\n", trait["id"].get<int>());
      full_names.push_back(create_trait_name(traits, trait));
    } else {
      full_names.push_back("");
    }
  }
  DEBUG_PRINTF("Finished generating trait names.\n");
  return full_names;
}
json normalize_traits(json& traits){
  auto cmp = [](const json& t1, const json& t2){
    return t1["id"].get<int>() < t2["id"].get<int>();
  };
  json::array_t& arr = traits.get_ref<json::array_t>();
  DEBUG_PRINTF("sorting traits.\n");
  std::sort(arr.begin(), arr.end(), cmp);
  int traits_max = arr.back()["id"].get<int>();
  json::array_t ret(traits_max+1);
  DEBUG_PRINTF("Creating new trait array.\n");
  for(auto&& trait : arr){
    int id = trait["id"].get<int>();
    ret[id] = std::move(trait);
  }
  DEBUG_PRINTF("Created new trait array.\n");
  return ret;
}
/*
  What we could do is decompress the data in chunks as we read it
  from the connection/socket/BIO/whatever. It would be a lot more
  complex, but we'd avoid having to store the whole gzip file in memory.
*/
//Decompress the gzip (or zlib) compressed data in 'in' into 'out', if
//using gzip'd data decompression will usually be done in one step.
//returns Z_OK on success and a zlib error code in case of error.
int decompress(const util::svector<char>& in, util::svector<char>& out){
  int decompressed_size;
  unsigned char *in_ptr = (unsigned char*)(in.data());
  //Check if in is a gzip stream using the magic number 1f 8b.
  if(in_ptr[0] == 0x1f && in_ptr[1] == 0x8b){
    //gzip stores the uncompressed size of the data (mod 2^32) as a 32 bit
    //little endian integer in the last 4 bytes of the data.
    const char *sz_ptr = in.data() + (in.size()-4);
    //Play nice with strict aliasing rules by using memcpy
    //decompressed_size = *((int*)sz_ptr); //breaks aliasing rules.
    memcpy(&decompressed_size, sz_ptr, sizeof(int));
  } else {
    //just make a rough guess, in most cases this is probably too small.
    //But we're not likely to get here anyway.
    decompressed_size = in.size() * 2;
  }
   
  out.reserve(decompressed_size);

  z_stream zlib_stream;
  //Initalize pointers for internal use in z_stream to NULL.
  zlib_stream.zalloc = nullptr;
  zlib_stream.zfree = nullptr;
  zlib_stream.opaque = nullptr;
  //Initialize input/output pointers/sizes
  zlib_stream.next_in = in_ptr;
  zlib_stream.avail_in = in.size();
  zlib_stream.next_out = (unsigned char*)(out.data());
  zlib_stream.avail_out = decompressed_size;  
  //The 15+16 means use a window size of 2^15 and assume the stream is 
  //gzip encoded.
  int ret = inflateInit2(&zlib_stream, 15 + 16);
  if(ret != Z_OK){
    return ret;
  }
  ret = inflate(&zlib_stream, Z_FINISH);
  if(ret == Z_STREAM_END){
    out.set_length(zlib_stream.total_out);
    inflateEnd(&zlib_stream);
    return Z_OK;//Z_OK == 0
  }
  //if ret == Z_BUF_ERROR it means the buffer wasn't big enough, this
  //shouldn't happen but we can still deal with it, any other value is an error.
  if(ret != Z_BUF_ERROR){ goto error; }
  fprintf(stderr, "Warning gzip stream larger than expected\n");
  do {
    //just double the size, this shouldn't happen so I'm not too worried about
    //memory usage here.
    int old_size = out.size();
    out.reserve(old_size * 2);
    zlib_stream.next_out = reinterpret_cast<unsigned char*>(out.data() + old_size);
    zlib_stream.avail_out = old_size;
    ret = inflate(&zlib_stream, Z_SYNC_FLUSH);
  } while(ret == Z_OK);
  if(ret != Z_STREAM_END){ goto error; }
  
  out.set_length(zlib_stream.total_out);
  return Z_OK;
 error:
    fprintf(stderr, "Error decompressing gzip stream : %s\n", zlib_stream.msg);
    inflateEnd(&zlib_stream);
    return ret;
}
//return number of tags entered, which should be either all or none,
//but I suppose it's possible to get an error midway.
int insert_tags(sqlite3_wrapper& db){
  int err = -1;
  DEBUG_PRINTF("Compiling insert statement.\n");
  sqlite3_stmt_wrapper ins_stmt = db.prepare_stmt(sql_insert_tag);
  if(!ins_stmt){
    db.print_errmsg("Error compiling sql");
    return err;
  }
  //Download tags file
  http_connection conn;
  if(!conn){
    fprintf(stderr, "Error establishing http connection");
    return -1;
  }
  util::svector<char> tags_gz;
  DEBUG_PRINTF("Downloading tags file.\n");
  if((err = conn.http_get(tags_uri, tags_gz)) != 0){
    fprintf(stderr, "Error downloading tags file : %d\n", err);
    return err;
  }
// In case you want to write the tags to file
//  FILE_wrapper tags_gz_out("vndb_tags.gz", "w");
//  tags_gz_out.write(tags_gz.data(), tags_gz.size());
  DEBUG_PRINTF("File downloaded, now decompressing.\n");
  util::svector<char> tags_text;
  if((err = decompress(tags_gz, tags_text)) != Z_OK){
    //some zlib error codes are positive (specifically Z_NEED_DICT)
    return (err > 0 ? -err : err);
  }
  DEBUG_PRINTF("File decompressed, now parsing.\n");
  json tags_json = json::parse(tags_text);
  json::array_t& tags = tags_json.get_ref<json::array_t>();
  db.begin_transaction();
  for(auto&& tag : tags){
    int idx = 1;
    ins_stmt.bind(idx++, tag["id"].get<int>());
    ins_stmt.bind(idx++, tag["name"].get<std::string_view>());
    ins_stmt.bind(idx++, tag["description"].get<std::string_view>());
    ins_stmt.bind(idx++, tag["meta"].get<bool>());
    ins_stmt.bind(idx++, tag["vns"].get<int>());
    ins_stmt.bind(idx++, tag["cat"].get<std::string_view>());
    ins_stmt.bind(idx++, tag["aliases"]);
    ins_stmt.bind(idx++, tag["parents"]);
    if(ins_stmt.exec() != SQLITE_OK){
      db.rollback_transaction();
      db.print_errmsg("Error inserting tag");
      return db.errcode();
    }
  }
  db.commit_transaction();
  DEBUG_PRINTF("Succeeded in inserting tags\n"); 
  return tags.size();
}    
int insert_traits(sqlite3_wrapper& db){
  int err = -1;
  DEBUG_PRINTF("Compiling insert statement.\n");
  sqlite3_stmt_wrapper ins_stmt = db.prepare_stmt(sql_insert_trait);
  if(!ins_stmt){
    db.print_errmsg("Error compiling sql");
    return err;
  }
  //Download traits file.
  http_connection conn;
  if(!conn){
    fprintf(stderr, "Error establishing http connection");
    return -1;
  }
  util::svector<char> traits_gz;
  DEBUG_PRINTF("Downloading traits file.\n");
  if((err = conn.http_get(traits_uri, traits_gz)) != 0){
    fprintf(stderr, "Error downloading traits file : %d\n", err);
    return err;
  }
//  FILE_wrapper traits_gz_out("vndb_traits.gz", "w");
//  traits_gz_out.write(traits_gz.data(), traits_gz.size());
  DEBUG_PRINTF("File downloaded, now decompressing.\n");
  util::svector<char> traits_text;
  if((err = decompress(traits_gz, traits_text)) != Z_OK){
    //some zlib error codes are positive (specifically Z_NEED_DICT)
    return (err > 0 ? -err : err);
  }
  DEBUG_PRINTF("File decompressed, now parsing.\n");
  json traits_json = json::parse(traits_text);
  traits_json = normalize_traits(traits_json);
  std::vector<std::string> trait_names = generate_trait_names(traits_json);
  json::array_t& traits = traits_json.get_ref<json::array_t>();
  db.begin_transaction();
  for(size_t i = 0; i < traits.size(); i++){
    if(traits[i].is_null()){ continue; }
    int idx = 1;
    ins_stmt.bind(idx++, traits[i]["id"].get<int>());
    ins_stmt.bind(idx++, traits[i]["name"].get<std::string_view>());
    ins_stmt.bind(idx++, static_cast<std::string_view>(trait_names[i]));
    ins_stmt.bind(idx++, traits[i]["description"].get<std::string_view>());
    ins_stmt.bind(idx++, traits[i]["meta"].get<bool>());
    ins_stmt.bind(idx++, traits[i]["chars"].get<int>());
    ins_stmt.bind(idx++, traits[i]["aliases"]);
    ins_stmt.bind(idx++, traits[i]["parents"]);
    if(ins_stmt.exec() != SQLITE_OK){
      db.rollback_transaction();
      db.print_errmsg("Error inserting trait");
      return db.errcode();
    }
  }
  db.commit_transaction();
  DEBUG_PRINTF("Succeeded in inserting traits\n"); 
  return traits.size();
}    
int main(){
  vndb_log = std::make_unique<util::logger>(default_log_file, util::log_level::debug);
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Error initializing ssl context.\n");
    return 1;
  }
  atexit(free_vndb_ssl_ctx);


  sqlite3_wrapper db(default_db_file);
  if(!db){
    fprintf(stderr, "Error opening sqlite database.\n");
    return 1;
  }
  if(db.exec_file(db_init_file) != SQLITE_OK){
    fprintf(stderr, "Error initializing database.\n");
    return 1;
  }
  //return insert_tags(db);
  return insert_traits(db);
}
