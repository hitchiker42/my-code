#!/bin/bash
echo "//This file which contains several other files concaenated together because of
//the constraints imposed on submitting programs. There is a comment before
//each file identifying the original file name." > translateBinary.c
echo "//#include \"vm.h\"" >> translateBinary.c
cat vm.h >> translateBinary.c
echo "//#include \"vm_translate.h\"" >> translateBinary.c
cat vm_translate.h >> translateBinary.c
echo "//#include \"vm_translate.c\"" >> translateBinary.c
cat vm_translate.c >> translateBinary.c
echo "//#include \"translate_binary.h\"" >> translateBinary.c
cat translate_binary.c >> translateBinary.c

