#include "alarmd.h"
#include <getopt.h>
/* User Interface to alarmd daemon
 * Basically multiple programs in one (I suppose I could compile it as multiplle
 * programs but I'm too lazy, maybe I will eventually
 *
 * Takes a command and essentially runs a different program for each command
 * Each command has it's own set of arguments, etc..
 * It's kinda like git (i.e git add,git commit, git clone,etc)
 *
 * Commands Are:
 * add: add a new alarm
 * list: list all alarms (prints a unique id number for each alarm
 * which is used to identify an alarm in other commands
 * delete: delete an alarm specified by it's id number
 * modify: change parameters(time,repeate date,command)
*/
static pid_t daemon_pid;
void __attribute__((noreturn)) help(){
  exit(EXIT_SUCCESS);
}
int main(int argc,char *argv[]){
  return 0;
}
