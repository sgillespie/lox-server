# Lox Server

> A language server implementation (LSP) for the Lox programming language.

Lox is the programming language implemented in the book *Crafting Interpreters*, by Robert Nystrom

## Installation

Lox Server can be installed by the following methods:

 * From release binaries
 * With Nix
 * With Haskell Tooling

### From Binaries

Prebuild binaries can be found on the latest
[release](https://github.com/sgillespie/lox-server/releases/latest).  Download the correct
archive from the _Assets_ section according to your operating system and system
architecture.

Extract the files to a directory referenced by your `PATH` environment variable.

### With Nix

On nix, you can run

    nix profile install github:sgillespie/lox-server/v0.1.0.0

### With Haskell Tooling

If you have the GHC toolchain installed, you can run

    cabal build all
    cabal install

## Usage

On emacs (eglot), you must add the Lox language server to `eglot-server-programs`. By default,
`lox` files will open in `fundamental-mode`:

    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
        '(fundamental-mode . ("lox-server"))))

When you open a lox file, start eglot: `M-x eglot`.

I have no idea how to do this in vim or Visual Studio Code. PRs welcome!

## Acknowledgements

The Lox language is from [Crafting
Interpreters](https://craftinginterpreters.com/) by [Robert Nystrom](https://github.com/munificent).

## License

This project is licensed under the BSD-3 license. Please see [LICENSE](LICENSE) for
details
