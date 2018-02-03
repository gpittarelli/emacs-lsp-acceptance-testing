
.PHONY: update
update:
	for f in emacs/*; do pushd $$f; git pull origin master; popd; done
