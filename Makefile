SHELL=bash
red=`tput setaf 1``tput setab 7`
reset=`tput sgr0`
TESTS_RULES = $(shell ls necro-test/*.sk)
TESTS = $(addprefix test/,$(shell ls test))

NAME := necro-ml
# WARNING : GIT TAG is prefixed with "v"
VERSION := 2.8.0.1
TGZ := $(NAME)-v$(VERSION).tar.gz
MD5TXT := $(TGZ).md5sum.txt
OPAM_NAME := necroml

# GIT VARIABLES
GITLAB := gitlab.inria.fr
GITPROJECT := skeletons
GIT_REPO := git@$(GITLAB):$(GITPROJECT)/$(NAME).git
GIT_RELEASE := https://$(GITLAB)/$(GITPROJECT)/$(NAME)/-/archive/v$(VERSION)/$(TGZ)

# OPAM VARIABLES
OPAM_REPO_GIT := ../opam-repository
OPAM_FILE := packages/$(OPAM_NAME)/$(OPAM_NAME).$(VERSION)/opam

##########################################################################################
#                       TARGET                                                           #
##########################################################################################

nothing:

# Change version number everywhere where it's specified, and update Makefile
new_version:
	@rm -f $(TGZ) $(MD5TXT)
	@read -p "Please enter new version (current is $(VERSION)): " g;\
	sed -i -r "s#archive/v$(VERSION)#archive/v$$g#" ./necroml.opam.template;\
	sed -i -r "s#v$(VERSION).tar.gz#v$$g.tar.gz#" ./necroml.opam.template;\
	sed -i -r "s/^VERSION := $(VERSION)/VERSION := $$g/" ./Makefile;\
	sed -i -r "s/^\(version $(VERSION)\)$$/\(version $$g\)/" ./dune-project;\
	git tag -a v$$g -fm 'OPAM necroml package for deployment'
	git push --follow-tags

opam_update :
	@rm -f $(TGZ) $(MD5TXT)
	@wget -q $(GIT_RELEASE)
	@md5sum $(TGZ) > $(MD5TXT)
	@sed -i -r 's|^(\schecksum: "md5\s*=\s*).*"|\1'$$(cat $(MD5TXT) | cut -d" " -f 1)'"|' necroml.opam.template
	@dune build
	@mkdir -p $(OPAM_REPO_GIT)/packages/$(OPAM_NAME)/$(OPAM_NAME).$(VERSION)/
	@cp necroml.opam $(OPAM_REPO_GIT)/$(OPAM_FILE)

release:
	make new_version
	make opam_update
	cd ../opam-repository; \
		git add packages/necroml && git commit -am "Push new Necro ML version" && git push

.PHONY: opam_update new_version release nothing
