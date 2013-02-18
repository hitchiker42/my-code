#global make file
SHELL= /bin/bash
CC?=gcc -O2
SUBDIRS= asm C D haskell java lisp ocaml python ruby sml
CLEANDIRS=$(SUBDIRS)
.PHONY: $(SUBDIRS)
all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
clean: $(CLEANDIRS)

$(CLEANDIRS):
	$(MAKE) -C $@ clean

