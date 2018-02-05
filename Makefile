
.PHONY: update
update:
	ls -d emacs/* | xargs --max-procs=1 -L1 -I "{}" git -C "{}" pull origin master

.PHONY: status
status:
	ls -d emacs/* | xargs -L1 -I "{}" git -C {} status
