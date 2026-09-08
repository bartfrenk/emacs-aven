PACKAGE       := aven
LOCAL_REPO    := emacs-aven
STRAIGHT_DIR  := $(HOME)/.config/emacs/.local/straight
STRAIGHT_REPO := $(STRAIGHT_DIR)/repos/$(LOCAL_REPO)
STRAIGHT_BUILD := $(STRAIGHT_DIR)/build/$(PACKAGE)

.PHONY: install

install:
	rm -rf $(STRAIGHT_BUILD)
	ln -sfn $(CURDIR) $(STRAIGHT_REPO)
	doom sync
