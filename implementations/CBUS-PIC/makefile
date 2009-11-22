dirs = canldr canrsusb 

.PHONY: ver.h all
all:
	$(foreach dir,$(dirs),$(MAKE) -C $(dir) $(newline))

.PHONY: clean
clean:
	$(foreach dir,$(dirs),$(MAKE) clean -C $(dir) $(newline))


define newline


endef

