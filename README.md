# Acceptance test suite for emacs-lsp

[lsp-mode](https://github.com/emacs-lsp/lsp-mode) is great. But LSP
servers behave in plenty of weird ways. This repo provides
edge-to-edge tests of various LSP servers talking to lsp-mode.

## Structure

 - `emacs/` stores submodules for lsp-mode and its various
   per-language extensions.
 - `servers/` stores submodules/installs for particular LSP servers.
 - `tests/` stores the scripts for actually running the e2e tests.

## License

Released under the GPLv3 License. See the LICENSE file for full text.

Copyright © 2018 George Pittarelli
