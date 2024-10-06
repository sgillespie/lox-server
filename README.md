# Obloxious

> A language server implementation (LSP) for the Lox programming language.

Lox is the programming language implemented by Crafting Interpreters, by Robert Nystrom

## Installation

On nix, you can run

    nix profile install

Otherwise, use cabal-install

    cabal build all
    cabal install

## Usage

On emacs (eglot), you must add the Lox language server to `eglot-server-programs`. By default,
`lox` files will open in `fundamental-mode`:

    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
        '(fundamental-mode . ("obloxious"))))

I have no idea how to do this in vim or Visual Studio Code. PRs welcome!

## Acknowledgements

The Lox language is from [Crafting
Interpreters](https://craftinginterpreters.com/) by [Robert Nystrom](https://github.com/munificent).
In addition, this language server reuses many ideas from the book.

## License

This project is licensed under the BSD-3 license. Please see [LICENSE](LICENSE) for
details
