dirs = canldr canrsusb canacc canace3 canled canrfid canlcd canxnet cancs

.PHONY: all
all:
	$(foreach dir,$(dirs),$(MAKE) -C $(dir) $(newline))

.PHONY: clean
clean:
	$(foreach dir,$(dirs),$(MAKE) clean -C $(dir) $(newline))


define newline


endef

