# kaocha-runner.el

An emacs package for running Kaocha tests via CIDER.

## Installation

I highly recommend installing kaocha-runner through elpa.

It should soon be available on [melpa](http://melpa.milkbox.net/):

    M-x package-install kaocha-runner

In the meantime, you're set if you have `s`, `cider`, `edn`.

Note that you must include kaocha in your dev dependencies for this to run in
the repl. The kaocha docs suggests creating a separate `:kaocha` profile, but
skip that if you want to run it from the repl.

## Usage

Kaocha runner exposes three commands:

- `kaocha-run-tests`

   Runs tests in the current namespace. With a prefix argument, it runs all tests.

- `kaocha-show-warnings`

   If you get any warnings from Kaocha, the report will just say *1 warning*.
   You can display this warning with this command. Given a prefix argument, it
   displays the warnings in a separate window.

- `kaocha-hide-windows`

   When displaying results, kaocha-runner pops up a window. This command hides
   it again. You can also switch to the window and kill it normally, if you
   prefer that.

## Keybindings

Pick your own. Here are mine:

```cl
(define-key clojure-mode-map (kbd "C-c k r") 'kaocha-run-tests)
(define-key clojure-mode-map (kbd "C-c k w") 'kaocha-show-warnings)
(define-key clojure-mode-map (kbd "C-c k h") 'kaocha-hide-windows)
```

## Configuration

By default, kaocha-runner does this when running tests:

```clj
(do (require 'kaocha.repl) (kaocha.repl/run))
```

In other words, it does not evaluate your code in any way. You'll have to
evaluate the code first, with a `(reset)` or just `C-c C-k` in the buffer.

To remedy this, you can change the `kaocha--repl-invocation-template` to include
a reset of your choice.

Also, if you want to shave ~150ms from each test run, you can remove the require
from the template. In that case, you'll have to require it yourself.

## License

Copyright (C) 2019 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.