'mkdir $(find  -maxdepth 1 -type f -printf %f"\n" | perl -ne \'print(m/[[{](\w+)([- ]\w+)[]}]/,"\n")\' | sort | uniq -d | sed \'s/ /_/g\')
