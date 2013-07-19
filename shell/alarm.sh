#! /bin/bash
# Example input and output (from the bash prompt):
# ./parse.bash -a par1 'another arg' --c-long 'wow!*\?' -cmore -b " very long "
# Option a
# Option c, no argument
# Option c, argument `more'
# Option b, argument ` very long '
# Remaining arguments:
# --> `par1'
# --> `another arg'
# --> `wow!*\?'

# Note that we use `"$@"' to let each command-line parameter expand to a
# separate word. The quotes around `$@' are essential!
# We need TEMP as the `eval set --' would nuke the return value of getopt.
usage(){
    echo "Usage: alarm.sh [-h|--help] [-ln|--loop=n (3)] [-f|--file filename] [-t timespec] COMMAND"
    echo -e "\t -h|--help, print this help and exit"
    echo -e "\t -ln|--loop=n, play song n times (default 3)(no space between l & n)"
    echo -e "\t -f|--file filename, use filename as song"
    echo -e "\t -t|--timespec timespec, timespec for cron\n\t\tdetermines when to set off the alarm"
    echo -e "\t Commands: stop/-s, stop current alarm\n\tprint, print a list of current alarms"
    return 0
}
print_alarms(){
    [[ -f ${HOME}/.alarms ]] && cat ${HOME}/.alarms && return 0
    echo "No alarms currently set"
}
if [[ $1 = "stop" ]] || [[ $1 = "-s" ]] ; then
    mocp -s
    return 0
fi

[[ $# != 0 ]] || usage()
MY_OPTS=$(getopt -o hl::f:t: --long help,loop::,file:,timespec \
     -n 'alarm.sh' -- "$@")

[[ $? != 0 ]] && echo "Terminating..." >&2 ; exit 1

# Note the quotes around `$TEMP': they are essential!
# This sets $1 to the first arg, even if it is an option
eval set -- "$MY_OPTS"

while true ; do
    case "$1" in
	-f|--file) ALARM_FILE="$2"; shift 2;;
	-t|--timespec) ALARM_TIME="$2" ; shift 2 ;;
	-l|--loop)
	    # loop has an optional argument. As we are in quoted mode,
	    # an empty parameter will be generated if its optional
	    # argument is not found.
	    case "$2" in
		"") ALARM_LOOP=3; shift 2 ;;
		*)  echo "Option c, argument \`$2'" ; shift 2 ;;
	    esac ;;
        -h|--help) usage();;
	--) shift ; break ;;
	*) echo "Internal error!" ; exit 1 ;;
    esac
done
#backup/create crontab
[[ -f ${HOME}/.crontab ]] || touch ${HOME}/.crontab
[[ -f ${HOME}/.crontab-backup ]] || cp ${HOME}/.crontab ${HOME}/.crontab-backup
#file to hold current alarms (needed for updating)
[[ -f ${HOME}/.alarms ]] || touch ${HOME}/.alarms
#playing w/o modifying playlist mocp -l song
#ALARM_CMD(){
#    LOOP=0
#    while [[ $LOOP -eq $ALARM_LOOP ]]; do
#        LOOP+=1
        
