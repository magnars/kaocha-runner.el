# kaocha-runner.el

An emacs package for running Kaocha tests via CIDER.

**Breaking:** Commands in this package are now prefixed with `kaocha-runner-`
instead of just `kaocha-`. Update your keybindings accordingly.

## Installation

I highly recommend installing kaocha-runner through elpa.

It is available on [melpa](http://melpa.milkbox.net/):

    M-x package-install kaocha-runner

or

    (use-package kaocha-runner
      :after (cider-mode))

Note that you must include kaocha in your dev dependencies for this to run in
the repl. The kaocha docs suggests creating a separate `:kaocha` profile, but
skip that if you want to run it from the repl.

## Usage

Kaocha runner exposes the following commands:

- `kaocha-runner-run-test-at-point`

   Runs the test at point in the current namespace.

- `kaocha-runner-run-tests`

   Runs tests in the current namespace. With a prefix argument, it runs test id provided by the user.

- `kaocha-runner-run-all-tests`

   Runs all tests in the project.

- `kaocha-runner-show-warnings`

   If you get any warnings from Kaocha, the report will just say *X warnings*.
   You can display them with this command. Given a prefix argument, it
   displays the warnings in a separate window.

- `kaocha-runner-hide-windows`

   When displaying results, kaocha-runner pops up a window. This command hides
   it again. You can also switch to the window and kill it normally, if you
   prefer that.

## Keybindings

Pick your own. Here are mine:

```cl
(define-key clojure-mode-map (kbd "C-c k t") 'kaocha-runner-run-test-at-point)
(define-key clojure-mode-map (kbd "C-c k r") 'kaocha-runner-run-tests)
(define-key clojure-mode-map (kbd "C-c k a") 'kaocha-runner-run-all-tests)
(define-key clojure-mode-map (kbd "C-c k w") 'kaocha-runner-show-warnings)
(define-key clojure-mode-map (kbd "C-c k h") 'kaocha-runner-hide-windows)
```

With `use-package`, something like:

```cl
(use-package kaocha-runner
  :after (cider-mode)
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))
```

## Configuration

The way kaocha-runner invokes kaocha is guided by two custom vars:

- `kaocha-runner-repl-invocation-template`

    which defaults to `(do (require 'kaocha.repl) %s)`. The `%s` is replaced
    with either `(kaocha.repl/run ...)` or `(kaocha.repl/run-all ...)`

- `kaocha-runner-extra-configuration`

    which defaults to `{:kaocha/fail-fast? true}`.

So by default, kaocha-runner does this when running tests:

```clj
(do (require 'kaocha.repl) (kaocha.repl/run {:kaocha/fail-fast? true}))
```

You can change this by changing the custom vars mentioned above.

Note that kaocha-runner *does not evaluate your code in any way*. You'll have to
evaluate the code first, with a `(reset)` or just `C-c C-k` in the buffer.

To remedy this, you can change the `kaocha-runner-repl-invocation-template` to
include a reset of your choice.

Also, if you want to shave ~150ms from each test run, you can remove the require
from the template. In that case, you'll have to require it yourself.

### Visual customisation

If the default red/green/yellow doesn't work for you, the faces are customisable:

- `kaocha-runner-success-face`

    Face used to highlight success messages.

- `kaocha-runner-error-face`

    Face used to highlight error messages.

- `kaocha-runner-warning-face`

    Face used to highlight warning messages.

You can also customise the tests, failure, and output window heights by setting
the following custom vars to the desired value:

- `kaocha-runner-ongoing-tests-win-min-height`

    The minimum height in lines of the output window when tests are taking long
    to run. This is to show the ongoing progress from kaocha.

- `kaocha-runner-failure-win-min-height`

    The minimum height in lines of the output window when there are failing tests.

- `kaocha-runner-output-win-max-height`

    The maximum height in lines of the output window.

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
