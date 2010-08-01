
SUBDIRS := $(foreach V,$(wildcard */Makefile),$(V:/Makefile=))

#.PHONY: clean $(foreach v,$(SUBDIRS),$(v).clean)
all:   $(foreach v,$(SUBDIRS),$(v).all)
        
%.all:
	make -C $(@:.all=) all 

