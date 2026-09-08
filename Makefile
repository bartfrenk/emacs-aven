PACKAGE       := aven
LOCAL_REPO    := emacs-aven
EMACS_VERSION := $(shell emacs --version | head -n1 | sed -E 's/GNU Emacs ([0-9]+\.[0-9]+).*/\1/')
STRAIGHT_DIR  := $(HOME)/.config/emacs/.local/straight
STRAIGHT_REPO := $(STRAIGHT_DIR)/repos/$(LOCAL_REPO)
STRAIGHT_BUILD := $(STRAIGHT_DIR)/build-$(EMACS_VERSION)/$(PACKAGE)

.PHONY: install reload

install:
	rm -rf $(STRAIGHT_BUILD)
	ln -sfn $(CURDIR) $(STRAIGHT_REPO)
	doom sync

reload: install
	emacsclient --eval '(kill-emacs)'
	emacs --daemon
