#/bin/bash
#rename all files in a directory to randomly chosen numbers
random() {
    head -n 1 /dev/urandom | od -N 4 | awk '{ print $2$3 }'
}
about() {
    echo -e 'copy all files in one directory to another directory and give them random names
Usage rand.sh -h [-cmu][-p path][-d dir][-n new_dir] 
\t-h help, print this help and exit
\t-c copy, if new-dir exists empty it and copy all files from dir, overides m and u
\t-m move, ignore any -d option, move to new-dir and randomize items
\t-u update, copy any items from dir to new-dir that are newer than then newest item in new-dir
\t-p path, base path to use for dir and new_dir, defaults to pwd, need to call before d&n
\t-d dir, directory to copy from, defaults to pwd #, need full path name
\t-n new_dir, new directory, defualts to ../tmp_dir #, need full path name
\tdefault options are to update and move any given option overrides this'
    exit 0
}
if [[ -z $1 ]]; then
    about
fi 
prefix=$PWD
while getopts ":chmup:d:n:" opt; do
    case "$opt" in
	"c" ) copy=true ;;
	"h" ) about ;;
	"m" ) move=true ;;
	"u" ) update=true ;;
	"p" ) prefix="$OPTARG" ;;
	"d" ) dir="$prefix/$OPTARG" ;;
	"n" ) new_dir="$prefix/$OPTARG" ;;
    esac
done
if [[ $copy != true ]] && [[ $move != true ]] && [[ $update != true ]]
then
    move=true && update=true && copy=false
fi
if [[ $move != true ]];then move=false;fi
if [[ $copy != true ]];then copy=false;fi
if [[ $update != true ]];then update=false;fi

if  [[ -z "$dir" ]]; then dir="$PWD"; fi
if  [[ -z "$new_dir" ]]; then new_dir="$dir/../tmp_dir"; fi
if  [[ ! -e "$new_dir" ]]; then mkdir "$new_dir"; fi
if  [[ ! -d "$new_dir" ]]; then exit 4; fi
cd "$new_dir"
if [[ $PWD != "$new_dir" ]]; then
    exit 3
fi
if [[ $copy == true ]]; then
    rm -f "$new_dir"/*
fi

if [[ $update == true ]]; then latest=`stat -c %Z \`ls -t | head -n 1\``; fi

if [[ $copy == true ]] || [[ $update == true ]]; then
    for i in `ls "$dir"`; do
	if [[ $copy == true ]] || [[ `stat -c %Z "$dir"/"$i"` -gt $latest ]]; then
	    cp "$dir"/"$i" "$PWD"
	fi
    done
fi
if [[ -e temp.txt ]]; then
    cat /dev/null >temp.txt
else
    touch temp.txt
fi

for i in `ls`; do
    if [[ "$i" == temp.txt ]]; then
	continue
    fi
    while true; do	
	x=`random`
	if [[ -e $x ]]; then
	    :
	else
	    cat temp.txt | grep -w $x >/dev/null
	    if [[ $? != 0 ]]; then
		break
	    else
		:
	    fi
	fi
    done
    echo $x >>temp.txt    
    mv "$i" $x
done
rm temp.txt
exit 0
