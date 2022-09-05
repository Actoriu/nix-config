DOTPATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
SHELL := /bin/bash
USER := $(shell whoami)

ifndef PLATFORM
	UNAME := $(shell uname -s)
	ifeq ($(UNAME), Darwin) # Darwin
		PLATFORM := mac
	else ifeq ($(UNAME), Linux) # Linux
		PLATFORM := linux
		ifneq (, $(wildcard /etc/arch-release)) # archlinux
			ifneq (, $(findstring arch,$(shell uname -r)))
				Host_Distro := archlinux
			endif
		else ifneq (, $(wildcard /etc/centos-release)) # Centos
			Host_Distro := centos
			Host_Version := $(shell grep -oE '[0-9]+\.[0-9]+' /etc/centos-release)
		else ifneq (, $(wildcard /etc/debian_version)) # Debian
			Host_Distro := debian
    else ifneq (, $(wildcard /etc/fedora-release)) # Fedora
			Host_Distro := fedora
			Host_Version := $(shell grep -oE '[0-9]+' /etc/fedora-release)
		else ifneq (, $(wildcard /etc/os-release)) # Debian or Ubuntu (debian or ubuntu)
			Host_Distro := $(shell grep "^ID=" /etc/os-release|awk -F= '{print $$2}')
			Host_Version := $(shell grep "^VERSION_ID=" /etc/os-release \
                        | awk -F= '{print $$2}' | sed -e 's/"//g')
		else ifneq (, $(wildcard /etc/oracle-release)) # Oracle Linux (OL)
			Host_Distro := ol
			Host_Version := $(shell grep -oE '[0-9]+\.[0-9]+' /etc/oracle-release)
		else ifneq (, $(wildcard /etc/redhat-release)) # Red Hat Enterprise Linux (RHEL)
			Host_Distro := rhel
			Host_Version := $(shell grep -oE '[0-9]+\.[0-9]+' /etc/redhat-release)
		else ifneq (, $(wildcard /etc/SuSE-release)) # SUSE Linux Enterprise Server (SLES)
			Host_Distro := sles
			Host_Version := $(shell awk '{if ($$1 == "VERSION") printf "%s.", $$3; \
                              else if ($$1 == "PATCHLEVEL") printf "%s", $$3}' \
                            /etc/SuSE-release)
		endif
	else
			PLATFORM := windows

			# uname returns:
			# CYGWIN: CYGWIN_NT-6.1
			# MSYS/MinGW: MINGW32_NT-6.2
			# MSYS2/MinGW-32:	MINGW32_NT-10.0
			# MSYS2/MinGW-64:	MINGW64_NT-10.0
			# MSYS2: MSYS_NT-10.0

			ifneq (, $(findstring CYGWIN,$(UNAME)))
				Host_Distro := cygwin
			endif

			ifneq (, $(findstring MINGW32,$(UNAME)))
				Mingw_Framework := 32
			else ifneq (, $(findstring MINGW64,$(UNAME)))
				Mingw_Framework := 64
			else ifneq (, $(findstring MSYS_NT,$(UNAME)))
				Mingw_Framework := 64
			endif

			# uname -o returns 'Msys' for both Msys and Msys2, so can't be used
			ifneq (, $(wildcard /msys.bat))
				MSYS_VERSION := 1
			else
				MSYS_VERSION := 2
			endif
	endif
endif

.DEFAULT_GOAL := help

all:

## Setup environment settings
init:

## Run make update, deploy, init
install: update deploy init
	@exec $$SHELL

## Fetch changes for this repo
update:
	git pull origin main
	git submodule init
	git submodule update
	git submodule foreach git pull origin main

## Remove the dot files and this repo
clean:
	@echo 'Remove dot files in your home directory...'
	-rm -rf $(DOTPATH)

# Inspired by <http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html>
# sed script explained:
# /^##/:
# 	* save line in hold space
# 	* purge line
# 	* Loop:
# 		* append newline + line to hold space
# 		* go to next line
# 		* if line starts with doc comment, strip comment character off and loop
# 	* remove target prerequisites
# 	* append hold space (+ newline) to line
# 	* replace newline plus comments by `---`
# 	* print line
# Separate expressions are necessary because labels cannot be delimited by
# semicolon; see <http://stackoverflow.com/a/11799865/1968>
## Self-documented Makefile
help:
	@echo "$$(tput bold)Available rules:$$(tput sgr0)"
	@echo
	@sed -n -e "/^## / { \
		h; \
		s/.*//; \
		:doc" \
		-e "H; \
		n; \
		s/^## //; \
		t doc" \
		-e "s/:.*//; \
		G; \
		s/\\n## /---/; \
		s/\\n/ /g; \
		p; \
	}" ${MAKEFILE_LIST} \
	| LC_ALL='C' sort --ignore-case \
	| awk -F '---' \
		-v ncol=$$(tput cols) \
		-v indent=19 \
		-v col_on="$$(tput setaf 6)" \
		-v col_off="$$(tput sgr0)" \
	'{ \
		printf "%s%*s%s ", col_on, -indent, $$1, col_off; \
		n = split($$2, words, " "); \
		line_length = ncol - indent; \
		for (i = 1; i <= n; i++) { \
			line_length -= length(words[i]) + 1; \
			if (line_length <= 0) { \
				line_length = ncol - indent - length(words[i]) - 1; \
				printf "\n%*s ", -indent, " "; \
			} \
			printf "%s ", words[i]; \
		} \
		printf "\n"; \
	}' \
| more $(shell test $(shell uname) == Darwin && echo '--no-init --raw-control-chars')

.PHONY: init install update clean help
