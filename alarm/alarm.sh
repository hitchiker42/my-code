#! /bin/bash
#figure out how to add some sort of test to stop an alarm
#i.e. enter a password, do a math problem etc
usage(){
    echo "Usage: alarm.sh [-h|--help]"
    echo -e "\t -h|--help, print this help and exit"
    exit 0
}
set-alarm(){
    at "$1" -f /home/tucker/.alarm
    if [[ $2 ]]; then
    at $(date -d "$1 $2 minutes" +%H:%M) -f /home/tucker/.alarm
    fi
}
unset-alarms(){
    $(atq | awk '{print $1}' | xargs atrm)
}
print-alarms(){
    atqueue=$(atq)
    if [[ "$atqueue" ]]; then
        if [[ $(echo "$atqueue" | grep -c '\n') -eq 1]]; then
            echo $(echo "$atqueue" |
                awk '{printf "An alarm is set for %s", $5}')
        else
#look at awk/gawk manpages to figure out how to print and before the last time
            echo "Alarms are set for:"
            echo $(echo "$atqueue" | awk '{printf "\t%s\n", $5}')
        fi
    else
        echo "No alarms are set"
    fi
}
[[ -z "$1" ]] && usage
TEMP=$(getopt -o s::,d:: --long snooze::,delay:: -n alarm -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -d|--delay)
            delay=t
            case "$2" in
                "") delay_time=5; shift 2;;
                *) delay_time="$2"; shift 2;;
            esac ;;
        -s|--snooze)
            snooze=t
            case "$2" in
                "") snooze_time=5; shift 2;;
                *) snooze_time="$2"; shift 2;;
            esac ;;
	--) shift ; break ;;
	*) echo "Internal error!" ; exit 1 ;;
    esac
done

if [[ $snooze ]]; then
    unset-alarms
    set-alarm $(date -d "$snooze_time minutes" +%H:%M)
    exit 0
fi
#...any other optional actions here(i.e. print alarms, remove alarms etc)
set-alarm "$1" "$delay_time"
