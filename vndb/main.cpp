#include "vndb.h"
#include "sql.h"//I Don't know if I'll need this or not.
//I'm just using getopt for now.
#include <getopt.h>
#include <signal.h>
std::unique_ptr<util::logger> vndb_log;
const char* current_log_file = default_log_file;
//TODO: add option to set log file name
static constexpr std::string_view usage_message =
R"EOF(./vndb_cpp [options] [command [command_arguments]]
  Options:
  global options: -u|--username name, -p|--password password, -d|--[no-]debug[=false],
                  --log-level level, -v|--verbose, -q|--quiet, -f|--db[-file] file
                  --[no-]gui[=sdl,fltk,none], -c|--[auto-]connect -h|--help
    username/password give the username and password for logging into vndb.
    log_level specifies how verbose logging should be.
    verbose causes logging to output to stderr rather than a file.
    debug is the same as --log_level debug, or --log_level warn if negated.
    quiet avoids displaying output unless there is an error.
    db-file specifies a specific file to use as the database (default is vn.db)
    [auto-]connect causes the program to attempt to connect to the vndb server
      on startup, and causes an error if it fails to do so.
    gui[=sdl,fltk,none] specify the gui front end or disable in altogether.
      With no argument the gui is set to sdl. If not given at all the
      gui is disabled, this will likely change.
      This argument only has an effect in interactive mode.
    help prints this message and exits.
  Commands:
  download/update [[main_]tables, [character_|vn_]images, tags, traits,
                   (vns|producers|characters|releases|staff)[=start],
                   vnlist, wishlist] [--no-derived-tables]
    download info from the vndb server, the only difference between download and
    update is the default starting id, for download it's 1 and for update its
    the current largest id. If any of the main tables or tags/traits are updated
    then any related derived tables will also be updated unless --no-derived-tables
    is given.
    (main_)tables downloads all of the vns, producers, staff, characters and releases,
      each of these can be specificed indivually as well in which case a specific
      start id can be given.
    images downloads both character and vn images, giving a prefix to image limits
      the kind of image to download.
    tags/traits downloads and parses the tag/trait dumps, theres no real way
      to update these incrementally.
    vnlist/wishlist download those, a username and password must be provided
      in this case.
  [re-]build[-derived-tables] [all, 'table_name']
    Explicitly re-build derived tables, by default all tables are rebuild,
    if specific table(s) are given only those table(s) are rebuilt.
  The following commands are currently unimplemented.
  eval "expr"
    evaluate the given expression.
  interactive | 'nothing'
    start interactive mode.)EOF";
//Unless I add a lot more commands a linear search (instead of a hash table/rbtree),
//should be fine.
static constexpr int num_commands = 6;
static constexpr std::array<std::string_view, num_commands> command_names = {{
    "download"sv, "update"sv, "build-derived-tables"sv,
    "re-build-derived-tables"sv, "evaluate"sv, "interactive"sv
}};
enum class command_type {
  download = 0,
  update = 1,
  build = 2,
  rebuild = 3,
  eval = 4,
  interactive = 5
};
[[noreturn]] void print_help_and_exit(){
  printf("%s\n", usage_message.data());
  exit(EXIT_SUCCESS);
}
int get_command_type(std::string_view command){
  for(int i = 0; i < num_commands; i++){
    if(command_names[i].size() < command.size()){ continue; }
    //Currently all commands have unique starting characters so a prefix
    //of a command is always unique if that changes this code needs to change.
    if(strncmp(command.data(), command_names[i].data(), command.size()) == 0){
      return i;
    }
  }
  return -1;
}
/*
  This function is pretty badly written, I'm going to have to re-write it,
  but it should work.
*/
int do_download_command(vndb_main& vndb, int argc,
                        char *const argv[], int arg_idx,
                        bool update, bool no_derived_tables){
  static constexpr int num_meta_arguments = 3;
  static constexpr std::array<std::string_view, num_meta_arguments> meta_arguments =
    {{"tables"sv, "main_tables"sv, "images"sv}};
  //A couple hacks
  static constexpr int image_argument_index = 2;
  static constexpr int vnlist_argument_idx =
    vndb_main::num_base_tables + num_meta_arguments;
  static constexpr int wishlist_argument_idx = vnlist_argument_idx + 1;

  if(arg_idx == argc){
    fprintf(stderr, "Error no arguments given to download command.\n");
    return EXIT_FAILURE;
  }
  const char* command_name = (update ? "update" : "download");
  std::array<std::string_view,
             vndb_main::num_base_tables + num_meta_arguments + 2> arguments;
  memcpy(arguments.data(), meta_arguments.data(),
         meta_arguments.size() * sizeof(std::string_view));
  memcpy(arguments.data() + num_meta_arguments, vndb_main::table_names.data(),
         vndb_main::num_base_tables * sizeof(std::string_view));
  //(hopefully) temporary hack since the vnlist & wishlist tables don't have
  //the same support as the other tables.
  arguments[vnlist_argument_idx] = "vnlist"sv;
  arguments[wishlist_argument_idx] = "wishlist"sv;
  util::array<int, vndb_main::num_base_tables> start_indexes(0);
  bool do_main_tables = false, do_tags = false, do_traits = false;
  bool do_vn_images = false, do_character_images = false;
  bool do_vnlist = false , do_wishlist = false;
  int default_start_idx = (update ? -1 : 1);
  vndb_log->log_debug("Parsing %s command arguments.\n", command_name);
  do {
    //find the end of the argument, either the null termintaor or an '=' if
    //it has a value attached to it.
    char *arg = argv[arg_idx];
    char *end = strchrnul(arg, '=');
    int idx = is_unique_prefix(std::string_view(arg, end - arg),
                               arguments.data(), arguments.size());
    vndb_log->log_debug("Parsing argument %s.\n", arg);
    if(idx < 0){
      if(idx == -2){
        fprintf(stderr, "Ambigous argument to %s command '%s'.\n",
                command_name, arg);
        //exit for now, we may just ignore the argument later.
        return EXIT_FAILURE;
      } else {
        //A special cases for vn/character_images, we want don't want these
        //to be included with the rest of the arguments since we want a prefix
        //of characters (and potentially vns) to refer to the base table.
        if(is_prefix_of(arg, "character_images")){
          do_character_images = true;
          continue;
        } else if(is_prefix_of(arg, "vn_images")){
          do_vn_images = true;
          continue;
        } else {
          fprintf(stderr, "Unknown argument to %s command '%s'.\n",
                  command_name, arg);
          return EXIT_FAILURE;
        }
      }
    }
    if(idx < num_meta_arguments){
      if(idx == image_argument_index){
        do_vn_images = do_character_images = true;
      } else { // main_tables
        do_main_tables = true;
        for(size_t i = 0; i < start_indexes.size(); i++){
          if(start_indexes[i] == 0){
            start_indexes[i] = default_start_idx;
          }
        }
      }
    } else {
      int what = idx - num_meta_arguments;
      if(what < vndb::num_object_types){//this is the target of a get command
        do_main_tables = true;
        int start = default_start_idx;
        if(*end == '='){
          start = strtol(end+1, nullptr, 0);
          if(start == 0){
            vndb_log->log_warn("Malformed or 0 start index '%s', using default (%d).\n",
                           end+1, default_start_idx);
            start = default_start_idx;
          }
        }
        start_indexes[what] = start;
      } else {
        switch(what){
          case to_underlying(vndb_main::table_type::tags):
            do_tags = true; break;
          case to_underlying(vndb_main::table_type::traits):
            do_traits = true; break;
          case vnlist_argument_idx:
            do_vnlist = true; break;
          case wishlist_argument_idx:
            do_wishlist = true; break;
          default:
            unreachable();
        }
      }
    }
  } while(++arg_idx < argc);
  // TODO: Split code above and below this line into seperate functions, above
  // is argument parsing and below is actually downloading and updating.
  //Login to the vndb server if necessary.
  if(do_main_tables || do_vnlist || do_wishlist){
    vndb_log->log_debug("Logging into vndb server.\n");
    vndb.connect(true);//make sure we're connected and have dbstats info
    vndb_log->log_debug("dbstats : '%s'.\n dbinfo : '%s'.\n",
                        vndb.db_stats.dump().c_str(),
                        vndb.db_info.dump().c_str());
  }
  vndb_log->log_debug("Begining to download data.\n");
  //TODO: add option to continue on error.
  if(do_main_tables){
    vndb_log->log_debug("Downloading data for main tables.\n");
    if(!vndb.download_all(start_indexes[0], start_indexes[1], start_indexes[2],
                          start_indexes[3], start_indexes[4])){
      fprintf(stderr, "Failed at downloading informaiton from vndb server.\n");
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Downloded data for main tables.\n");
  }

  //These next two issue their own error messages since they were originally
  //put in a seperate program.
  if(do_tags){
    vndb_log->log_debug("Downloading data for tags.\n");
    if(download_and_insert_tags(vndb.get_db()) < 0){
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Downloded data for tags.\n");
  }
  if(do_traits){
    vndb_log->log_debug("Downloading data for traits.\n");
    if(download_and_insert_traits(vndb.get_db()) < 0){
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Downloded data for traits.\n");
  }
  if(do_vnlist){
    fprintf(stderr, "Download vnlist is not yet implemented.\n");
    //Not written yet
    //vndb_log->log_debug("Downloded data for vnlist.\n");
  }
  if(do_wishlist){
    fprintf(stderr, "Download vnlist is not yet implemented.\n");
    //Not written yet
    //vndb_log->log_debug("Downloded data for wishlist.\n");
  }
  if(do_vn_images){
    vndb_log->log_debug("Downloading vn images.\n");
    bool success = (update ? vndb.update_vn_images() : vndb.build_vn_images());
    if(!success){
      fprintf(stderr, "Error downloading vn images.\n");
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Downloded vn images.\n");
  }
  if(do_character_images){
    vndb_log->log_debug("Downloading character images.\n");
    bool success = (update ? vndb.update_character_images() :
                    vndb.build_character_images());
    if(!success){
      fprintf(stderr, "Error downloading character images.\n");
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Downloded character images.\n");
  }

  if(!no_derived_tables && (do_main_tables || do_tags || do_traits)){
    bool err = false;
    if(do_main_tables || do_tags){
      err = !vndb.build_vn_tags();
    }
    if(!err && (do_main_tables || do_traits)){
      err = !vndb.build_character_traits();
    }
    if(!err && do_main_tables){
      err = !vndb.build_vn_producer_relations();
    }
    if(!err && do_main_tables){
      err = !vndb.build_staff_derived_tables();
    }
    if(err){
      fprintf(stderr, "Error when building derived tables.\n");
      return EXIT_FAILURE;
    }
    vndb_log->log_debug("Re-built derived tables.\n");
  }
  vndb_log->log_debug("dbinfo : '%s'.\n",
                      vndb.db_info.dump().c_str());
  return EXIT_SUCCESS;
}
/*
[[noreturn]] void run_interactively(vndb_main &vndb){
  set_close_on_exec_all();
  printf("Placeholder for interactive loop, pid = %d.\n"
         "Press any key to quit.\n", getpid());
  getchar();
  exit(EXIT_SUCCESS);
}
*/
void ignore_signal(int sig){
  vndb_log->log_debug("Igoring signal %d.\n", sig);
  //fprintf(stderr, "Igoring signal %d.\n", sig);
  return;
}
int main(int argc, char * const argv[]){
  int log_to_stderr = false, no_derived_tables = false, auto_connect = false;
  static const struct option options[] = {
    //options with a required argument
    {"username", required_argument, 0, 'u'},
    {"password", required_argument, 0, 'p'},
    {"log-level", required_argument, 0, 'l'},
    {"db-file", required_argument, 0, 'f'},
    //options with no argument
    {"connect", no_argument, 0, 'c'},
    {"auto-connect", no_argument, 0, 'c'},
    {"help", no_argument, 0, 'h'},
    {"verbose", no_argument, 0, 'v'},
    {"quiet", no_argument, 0, 'q'},
    //options with optional arguments
    //These can also be given as a negated version by prefixing 'no-',
    //the same value as the non-negated version is returned in this case
    //and we manually look at which option was passed.
    {"debug", optional_argument, 0, 'd'},
    {"no-debug", no_argument, 0, 'd'},
    {"gui", optional_argument, 0, 'g'},
    {"no-gui", no_argument, 0, 'g'},
    //Long options with no short option equivelent
    {"no-derived-tables", no_argument, &no_derived_tables, true},
    {0,0,0,0}
  };
  //Ignore sigpipe, openssl can cause this and it's not really an issue.
  struct sigaction ignore_act, old_act;
  ignore_act.sa_handler = ignore_signal;
  sigemptyset(&ignore_act.sa_mask);
  ignore_act.sa_flags = SA_RESTART;
  sigaction(SIGPIPE, &ignore_act, &old_act);
  //Set SIGCHLD to ignore so we don't need to reap any child processes
  ignore_act.sa_handler = SIG_IGN;
  sigaction(SIGCHLD, &ignore_act, &old_act);
  /*
    Avoid doing anything that might do logging untill we've
    finished parsing options, so we know what the log level should be.
  */
  const char *username = "", *password = "";
  const char* log_level_name = "debug";
  const char* db_filename = default_db_file;
  const char* gui_type_name = "none";
  opterr = 0; //prevent getopt from printing its own error messages.
  int c, options_idx = 0;
  while((c = getopt_long(argc, argv, ":u:p:d::g::l:qvc",
                         options, &options_idx)) >= 0){
    switch(c){
      case 'u':
        username = optarg;
        break;
      case 'p':
        password = optarg;
        break;
      case 'c':
        auto_connect = true;
        break;
      case 'd':{
        if(optarg){
          if(strncasecmp(optarg, "false", strlen(optarg)) == 0){
            log_level_name = "warn";
          } else if(strncasecmp(optarg, "true", strlen(optarg)) == 0){
            log_level_name = "debug";
          } else {
            //No reason to quit over this, but at least print a warning.
            fprintf(stderr, "Non-boolean argument given to debug option: %s\n",
                    optarg);
            log_level_name = "debug";
          }
        } else {
          if(strcmp(options[options_idx].name,"no-debug") == 0){
            log_level_name = "warn";
          } else {
            log_level_name = "debug";
          }
        }
        break;
      }
      case 'g':{
        if(optarg){
          gui_type_name = nullptr;
          for(auto opt : {"sdl", "fltk", "none"}){
            if(strncasecmp(optarg, opt, strlen(optarg)) == 0){
              gui_type_name = opt;
            }
          }
          if(!gui_type_name){
            //No reason to quit over this, but at least print a warning.
            fprintf(stderr, "Invalid argument given to gui option '%s'.\n"
                    "Defaulting to no gui.\n",
                    optarg);
            gui_type_name = "none";
          }
        } else {
          if(strcmp(options[options_idx].name,"no-gui") == 0){
            gui_type_name = "none";
          } else {
            gui_type_name = "sdl";
          }
        }
        break;
      }
      case 'h':
        print_help_and_exit();
      case 'l':
        log_level_name = optarg;
        break;
      case 'q':
        log_level_name = "error";
        break;
      case 'n':
        log_level_name = "warn";
        break;
      case 'v':
        log_to_stderr = true;
        break;
      case 't':
        no_derived_tables = true;
        break;
      case ':':
        fprintf(stderr, "Missing required option for argument '%s'.\n",
                argv[optind]);
      //0 is returned for options which set a flag.
      case 0:
        break;
    }
  }
  int log_level = util::position(util::log_level_names, log_level_name);
  if(log_level == -1){
    fprintf(stderr, "Unknown log level '%s'.\n Defaulting to debug.\n",
            log_level_name);
    log_level = to_underlying(util::log_level::debug);
  }
  if(log_to_stderr){
    vndb_log = std::make_unique<util::logger>(stderr, log_level);
  } else {
    //rename old log file, we only do this if not logging to stderr to avoid
    //removing an old log file unnecessarily.
    rename(default_log_file, default_log_file_bkup);
    vndb_log = std::make_unique<util::logger>(default_log_file, log_level);
  }
  if(!vndb_log->out){
    fprintf(stderr, "Failed to open log file \"%s\".\n", default_log_file);
  }
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Failed to initialize ssl context.\n");
    exit(EXIT_FAILURE);
  }
  atexit(free_vndb_ssl_ctx);
  vndb_log->log_debug("DEBUG | Initializing vndb_main object.\n");
  vndb_main vndb(db_filename, username, password);
  if(!vndb.init_all(auto_connect)){
    exit(EXIT_FAILURE);
  }
  int gui_type_val = util::position(vndb_main::gui_type_names, gui_type_name);
  vndb.gui = vndb_main::gui_type(gui_type_val);
  int arg_idx = optind;
  if(arg_idx == argc){
    run_interactively(vndb);
    //printf("%s\n", usage_message.data());
    // exit(EXIT_SUCCESS);
  }
  int cmd_type = get_command_type(argv[arg_idx]);
  if(cmd_type < 0){
    fprintf(stderr, "Unknown command type '%s'.\n", argv[arg_idx]);
    print_help_and_exit();
  }
  ++arg_idx;
  command_type cmd = (command_type)cmd_type;
  if(cmd == command_type::update || cmd == command_type::download){
    return do_download_command(vndb, argc, argv, arg_idx,
                               cmd == command_type::update, no_derived_tables);
  } else if(cmd == command_type::build || cmd == command_type::rebuild){
    //TODO: Actually parse the arguments to this command.
    vndb.build_derived_tables();
  } else if(cmd == command_type::eval){
    std::string command(argv[arg_idx]);
    if(command.back() != ';'){
      command.push_back(';');
    }
    return do_command(&vndb, command);
  } else if(cmd == command_type::interactive){
    run_interactively(vndb);
  } else {
    unreachable();
  }
}
