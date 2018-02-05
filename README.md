# Acceptance test suite for emacs-lsp

[lsp-mode](https://github.com/emacs-lsp/lsp-mode) is great. But LSP
servers behave in plenty of weird ways. This repo provides
edge-to-edge tests of various LSP servers talking to lsp-mode.

## Structure

 - `emacs/` stores submodules for lsp-mode and its various
   per-language extensions. Also, all of their dependencies are
   manually maintained as submodules to allow easily testing with any
   versions/commits.
 - `servers/` stores submodules/installs of particular LSP servers.
 - `tests/` stores the scripts for actually running the tests and
   any necessary fixture data.

## Usage

`make update` and `make status` do about what you'd expect for all the
submodules in `emacs/`.

Not really ready for general testing yet. But get all the submodules
to your desired versions and then run the test scripts with something like:

```
emacs -Q --batch -l tests/setup.el -l tests/lsp-javascript-flow.el
```

That will create a lsp-javascript-flow.json file which you can view
with viewer.html

## License

Released under the GPLv3 License. See the LICENSE file for full text.

Copyright © 2018 George Pittarelli
