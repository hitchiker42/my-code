#!/bin/bash
echo "//This file which contains several other files concaenated together because of
//the constraints imposed on submitting programs. There is a comment before
//each file identifying the original file name." > prog5_cat.c
echo "//#include \"prog5_macros.h\"" >> prog5_cat.c
cat prog5_macros.h >> prog5_cat.c
echo "//#include \"prog5.h\"" >> prog5_cat.c
cat prog5.h >> prog5_cat.c
echo "//#include \"prog5_consts.h\"" >> prog5_cat.c
cat prog5_consts.h >> prog5_cat.c
echo "//#include \"prog5_heap.c\"" >> prog5_cat.c
cat prog5_heap.c >> prog5_cat.c
echo "//#include \"prog5.c\"" >> prog5_cat.c
cat prog5.c >> prog5_cat.c
