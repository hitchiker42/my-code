#! /bin/bash
#figure out how to add some sort of test to stop an alarm
#i.e. enter a password, do a math problem etc
password="Stop Alarm"
usage(){
    echo "Usage: alarm.sh [-d|--delay] [-p|--password] [-s|--snooze] [-h|--help] time"
    echo -e "\t-d|--delay [MIN] set an additional alarm MIN minutes after the first, default 5"
    echo -e "\t-s|--snooze [MIN] stop alarm and set new alarm MIN minutes from present, default 5"
    echo -e "\t-p|--password password protect snooze and stop actions"
    echo -e "\t-q|--queue print the currently queued alarms, if any"
    echo -e "\t-h|--help, print this help and exit"
    echo -e "\tdefault behavior with no arguments is to print alarms if any, otherwise print help"
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
        return 0
    else
        echo "No alarms are set"
        return 2
    fi
}
change-password(){
# need to test this before I use it
#    sed "s/password=.*/password=$1/" > alarm.sh
}
password-unlock(){
    if [[ $password_protected ]]; then
        attempts=3
        while [[ attempts -gt 0]]; do
            read -s -p "Password: " guess
            if [[ "$guess" == "$password" ]]; then
                return 0
            else 
                printf "Sorry, try again, %d attempts left\n" $attempts
            fi
            attempts=$((attempts - 1))
        done
        echo "failed to enter password correctly after 3 attempts"
        return 1
    else
        return 0
    fi
}
stop-alarm(){
    password-unlock
    if [[ $? -eq 0 ]];then
        killall aplay
        exit 0
    else
        exit 1
    fi
}
snooze(){
    password-unlock
    if [[ $? -eq 0 ]];then
        unset-alarms
        set-alarm $(date -d "$1 minutes" +%H:%M)
        exit 0
    else
        exit 1
    fi
}
if [[ -z "$1" ]]; then
    temp=$(print-alarms)
    if [[ $? -eq 0 ]]; then
        echo $temp
        exit 0
    else
        usage()
    fi
fi
TEMP=$(getopt -o s::,d::,p::,q,h --long snooze::,delay::,password::,queue,help -n alarm -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -d|--delay)
            delay=t
            case "$2" in
                "") delay_time=5; shift 2;;
                *) delay_time="$2"; shift 2;;
            esac ;;
        -p|--password)
            password_protected=t
            case "$2" in
                "") shift 2;;
                *) change-password "$2"; shift 2;;
            esac ;;
        -s|--snooze)
            snooze=t
            case "$2" in
                "") snooze_time=5; shift 2;;
                *) snooze_time="$2"; shift 2;;
            esac ;;
        -q|--queue)
            print-alarms
            exit $? ;;
        -h|--help) usage ;;
	--) shift ; break ;;
	*) echo "Internal error!" ; exit 1 ;;
    esac
done

if [[ $snooze ]]; then
    snooze $snooze_time
fi
#...any other optional actions here(i.e. print alarms, remove alarms etc)
set-alarm "$1" "$delay_time"
