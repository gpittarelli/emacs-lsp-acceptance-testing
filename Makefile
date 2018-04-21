
.PHONY: update
update:
	ls -d emacs/* | xargs --max-procs=1 -L1 -I "{}" git -C "{}" pull origin master

.PHONY: status
status:
	ls -d emacs/* | xargs -L1 -I "{}" git -C {} status

.PHONY: test
test:
	ls tests/lsp-*.el | xargs -L1 -P4 emacs -Q --batch -l tests/setup.el -l
