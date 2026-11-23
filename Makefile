REPO_DIR := $(shell cd "$$(dirname "$(strip $(MAKEFILE_LIST))")" && pwd -P)
TARGET ?= $(HOME)
PLATFORM ?= auto
PACKAGES ?=
.DEFAULT_GOAL := help
.PHONY: help check deploy install doctor status uninstall
help check deploy install doctor status uninstall:
	@bash "$(REPO_DIR)/deploy.sh" $@ --home "$(TARGET)" --platform "$(PLATFORM)" $(if $(PACKAGES),--packages $(PACKAGES),)
