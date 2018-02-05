# Acceptance test suite for emacs-lsp

[lsp-mode](https://github.com/emacs-lsp/lsp-mode) is great. But LSP
servers behave in plenty of weird ways. This repo provides
edge-to-edge tests of various LSP servers talking to lsp-mode.

## Structure

 - `emacs/` stores submodules for lsp-mode and its various
   per-language extensions.
 - `servers/` stores submodules/installs for particular LSP servers.
 - `tests/` stores the scripts for actually running the e2e tests.

## Usage

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
