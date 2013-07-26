#/bin/bash
#rename all files in a directory to randomly chosen numbers
#Tucker Dinapoli (C) 2012-2013
random() {
    head -n 1 /dev/urandom | od -N 16 -x | awk '{ print $2$3$4$5 }'
}
about() {
    echo -e "copy all files in one directory to another directory and give them random names"
echo -e "Usage rand.sh -h [-cmu][-p|--path path][-d|--dir dir][-n|--new new_dir]"
echo -e "Options:"
echo -e "\t-h|--help, print this help and exit"
echo -e "\t-c|--copy, if new-dir exists empty it and copy all files from dir, overides r and u"
echo -e "\t-r|--rand, cd to new dir and randomize filenames"
echo -e "\twith update or copy perform those actions first before randomizing"
echo -e "\t-u|--update, copy any items from dir to new-dir that are newer than then newest item in new-dir"
echo -e "\t-p|--path PATH, base path to use for dir and new_dir, defaults to pwd"
echo -e "\t-d|--dir DIR, directory to copy from, defaults to pwd"
echo -e "\t-n|--new DIR, new directory, defualts to path/rand-(dir)"
echo -e "\t-e|--ext, keep filename extensions"
echo -e "\tunless --rand,--copy or --move given default to --rand and --move"
    exit 0
}
file_ext(){
    file_string=$(file -b "$1" | awk '{print $1}')
    case $file_string in
        PC) echo '.bmp';;
        JPEG) echo '.jpg';;
        PNG) echo '.png';;
    esac
}
VERSION=0.02
if [[ -z $1 ]]; then
    about
fi 
prefix=$PWD
start_dir=$PWD
TEMP=$(getopt -o c,h,m,u,p:,d:,n:,e\
 --long copy,help,move,update,prefix:,dir:,new_dir:,ext -n 'rand.sh' -- "$@")

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"
#get options
while true ; do
	case "$1" in
	    -c|--copy ) copy=t ; shift ;;
	    -h|--help ) about ;;
	    -r|--rand ) rand=t ; shift ;;
	    -u|--update ) update=t ; shift ;;
	    -p|--prefix ) prefix="$2" ; shift 2 ;;
	    -d|--dir ) dir="$2" ; shift 2 ;;
	    -n|--new ) new_dir="$2" ; shift 2 ;;
            -e|--ext ) ext=t ; shift ;;
            --) shift ; break ;;
            *) echo "Internal error!, remaning args $@" ; exit 1;;
        esac
done
#set default actions if no action explictly give
if [[ -z $copy ]] && [[ -z $move ]] && [[ -z $update ]]
then
    move=t && update=t
fi
#set paths of dir and new_dir
if [[ -z "$dir" ]]; then
    dir="$PWD"
else
    dir="$prefix"/"$dir"
fi
if  [[ -z "$new_dir" ]]; then
    new_dir="$prefix/rand-$(basename $dir)"
else
    new_dir="$prefix/$new_dir"
fi
#insure new_dir exists and cd into in
[[ ! -e "$new_dir" ]] && mkdir "$new_dir"
[[ ! -d "$new_dir" ]] && exit 4
cd "$new_dir"
[[ $PWD != "$new_dir" ]] && exit 3

#if we're copying we need to empty the current directory
[[ $copy ]] && rm -f "$new_dir"/*

#using explict timestamps
#if [[ $update == true ]] && [[ $(ls $new_dir) ]];then
#    latest=$(stat -c %Z $(ls -t | head -n 1))
#else
#    latest=$(($(stat -c %Z $(ls "$dir" -rt | head -n 1)) - 1))
#fi
#find the file with the oldest timestamp that we still want to copy
if [[ $update ]] && [[ "$(ls $new_dir)" ]]; then
#if we're running update its the latest file in new_dir
    latest=$(ls -t | head -n 1)
else
#else its the oldest file in dir
#THIS HAS MAJOR ISSUES
    latest=/bin/bash
#    cp "$$latest" "$PWD" #latest doesn't get copied in the loop
fi
#copy files
if [[ $copy ]] || [[ $update ]]; then
    for i in $(find $dir -newer "$latest"); do
	cp "$i" "$PWD"
    done
fi
#create temp file to hold filenames
if [[ -e temp.txt ]]; then
    cat /dev/null >temp.txt
else
    touch temp.txt
fi

for i in $(ls); do
#don't rename our temp file
    if [[ "$i" == temp.txt ]]; then
	continue
    fi
#loop untill we get a unique random filename
    while true; do
	x=$(random)
        if [[ 16 -ne ${#x} ]]; then
            :
        else
#	if [[ -e $x ]]; then #if x exists loop again
#	    :
#	else
#search for x in the temp file, make a new x untill we find a unique name
            grep -qw $x temp.txt
	    if [[ $? != 0 ]]; then
		break
	    else
		:
	    fi
	fi
    done
    echo $x >>temp.txt
#get file extension if necessary
    if [[ $ext ]];then cur_ext=$(file_ext "$i");fi
#change filname
    mv "$i" "$x$cur_ext"
done
rm temp.txt
echo "rand.sh finished successfully"
exit 0
