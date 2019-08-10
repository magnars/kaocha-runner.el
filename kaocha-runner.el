;;; kaocha-runner.el --- A package for running Kaocha tests via CIDER. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26") (s "1.4.0") (cider "0.21.0") (parseedn "0.1.0"))
;; URL: https://github.com/magnars/kaocha-runner.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor-mode for running Kaocha tests with CIDER

;;; Code:

(require 'cider)
(require 'parseedn)
(require 's)

(defun kaocha-runner--eval-clojure-code (code callback)
  "Send CODE to be evaled and run to CIDER, calling CALLBACK with updates."
  (cider-nrepl-request:eval
   code
   callback
   (cider-current-ns)
   nil nil nil
   (cider-current-repl nil 'ensure)))

(defvar kaocha-runner--out-buffer "*kaocha-output*")
(defvar kaocha-runner--err-buffer "*kaocha-error*")

(defun kaocha-runner--clear-buffer (buffer)
  "Ensure that BUFFER exists and is empty."
  (get-buffer-create buffer)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))))

(defun kaocha-runner--colorize ()
  "Turn ANSI codes in the current buffer into Emacs colors."
  (save-excursion
    (goto-char (point-min))
    (insert "[m")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun kaocha-runner--insert (buffer s)
  "Insert S into BUFFER, then turn ANSI codes into color."
  (with-current-buffer buffer
    (insert s)
    (kaocha-runner--colorize)))

(defmacro kaocha-runner--with-window (buffer original-buffer &rest body)
  "Open a dedicated window showing BUFFER, perform BODY, then switch back to ORIGINAL-BUFFER."
  (declare (debug (form body))
           (indent 2))
  `(let ((window (get-buffer-window ,buffer)))
     (if window
         (select-window window)
       (let ((window (split-window-vertically -4)))
         (select-window window)
         (switch-to-buffer ,buffer)
         (set-window-dedicated-p window t)))
     ,@body
     (switch-to-buffer-other-window original-buffer)))

(defun kaocha-runner--fit-window-snuggly (min-height max-height)
  "Resize current window to fit its contents, within MIN-HEIGHT and MAX-HEIGHT."
  (window-resize nil (- (max min-height
                             (min max-height
                                  (- (line-number-at-pos (point-max))
                                     (line-number-at-pos (point)))))
                        (window-height))))

(defun kaocha-runner--recenter-top ()
  "Change the scroll position so that the cursor is at the top of the window."
  (recenter (min (max 0 scroll-margin)
                 (truncate (/ (window-body-height) 4.0)))))

(defun kaocha-runner--num-warnings ()
  "Count the number of warnings in the error buffer."
  (s-count-matches "WARNING:"
                   (with-current-buffer kaocha-runner--err-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))))

(defun kaocha-runner--show-report (value current-ns)
  "Show a message detailing the test run restult in VALUE, prefixed by CURRENT-NS."
  (when-let* ((result (parseedn-read-str (s-chop-prefix "#:kaocha.result" value))))
    (let* ((tests (gethash :count result))
           (pass (gethash :pass result))
           (fail (gethash :fail result))
           (err (gethash :error result))
           (warnings (kaocha-runner--num-warnings))
           (happy? (and (= 0 fail) (= 0 err)))
           (report (format "[%s] %s"
                           current-ns
                           (propertize (format "%s tests, %s assertions%s, %s failures."
                                               tests
                                               (+ pass fail err)
                                               (if (< 0 err)
                                                   (format ", %s errors" err)
                                                 "")
                                               fail)
                                       'face (if happy?
                                                 '(:foreground "green")
                                               '(:foreground "red"))))))
      (when (< 0 warnings)
        (let ((warnings-str (format "(%s warnings)" warnings)))
          (setq report (concat report (s-repeat (max 3 (- (frame-width) (length report) (length warnings-str))) " ")
                               (propertize warnings-str 'face '(:foreground "yellow"))))))
      (message "%s" report))))

(defvar kaocha-runner--fail-re "\\(FAIL\\|ERROR\\)")

(defun kaocha-runner--show-details-window (original-buffer min-height)
  "Show details from the test run with a MIN-HEIGHT, but switch back to ORIGINAL-BUFFER afterwards."
  (kaocha-runner--with-window kaocha-runner--out-buffer original-buffer
    (visual-line-mode 1)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward kaocha-runner--fail-re nil t))
    (end-of-line)
    (kaocha-runner--fit-window-snuggly min-height 16)
    (kaocha-runner--recenter-top)))

(defcustom kaocha-runner--repl-invocation-template
  "(do (require 'kaocha.repl) %s)"
  "The invocation sent to the REPL to run kaocha tests, with the actual run replaced by %s.")

(defun kaocha-runner--run-tests (&optional run-all? background?)
  "Run kaocha tests.

If RUN-ALL? is t, all tests are run, otherwise just run tests in
the current namespace.

If BACKGROUND? is t, we don't message when the tests start running."
  (interactive)
  (kaocha-runner--clear-buffer kaocha-runner--out-buffer)
  (kaocha-runner--clear-buffer kaocha-runner--err-buffer)
  (kaocha-runner--eval-clojure-code
   (format kaocha-runner--repl-invocation-template (if run-all?
                                                       "(kaocha.repl/run-all)"
                                                     "(kaocha.repl/run)"))
   (let ((current-ns (cider-current-ns))
         (original-buffer (current-buffer))
         (done? nil)
         (any-errors? nil)
         (shown-details? nil)
         (the-value nil))
     (if run-all?
         (kaocha-runner--show-details-window original-buffer 12)
       (unless background?
         (message "[%s] Running tests ..." current-ns)))
     (lambda (response)
       (nrepl-dbind-response response (content-type content-transfer-encoding body
                                                    value ns out err status id)
         (when out
           (kaocha-runner--insert kaocha-runner--out-buffer out)
           (when (let ((case-fold-search nil))
                   (string-match-p kaocha-runner--fail-re out))
             (setq any-errors? t)))
         (when err
           (kaocha-runner--insert kaocha-runner--err-buffer err))
         (when value
           (setq the-value value))
         (when (and status (member "done" status))
           (setq done? t))
         (when done?
           (if the-value
               (kaocha-runner--show-report the-value current-ns)
             (unless (get-buffer-window kaocha-runner--err-buffer 'visible)
               (message "Kaocha run failed. See error window for details.")
               (switch-to-buffer-other-window kaocha-runner--err-buffer))))
         (when (and done? any-errors? (not shown-details?))
           (setq shown-details? t)
           (kaocha-runner--show-details-window original-buffer 4)))))))

(defun kaocha-runner-hide-windows ()
  "Hide all windows that kaocha has opened."
  (interactive)
  (when (get-buffer kaocha-runner--out-buffer)
    (kill-buffer kaocha-runner--out-buffer))
  (when (get-buffer kaocha-runner--err-buffer)
    (kill-buffer kaocha-runner--err-buffer)))

(defun kaocha-runner-run-tests (&optional run-all?)
  "Run tests in the current namespace.
Prefix argument RUN-ALL? runs all tests."
  (interactive "P")
  (kaocha-runner-hide-windows)
  (kaocha-runner--run-tests run-all?))

(defun kaocha-runner-show-warnings (&optional switch-to-buffer?)
  "Display warnings from the last kaocha test run.
Prefix argument SWITCH-TO-BUFFER? opens a separate window."
  (interactive "P")
  (if switch-to-buffer?
      (switch-to-buffer-other-window kaocha-runner--err-buffer)
    (message "%s"
             (s-trim
              (with-current-buffer kaocha-runner--err-buffer
                (buffer-substring (point-min) (point-max)))))))

(provide 'kaocha-runner)
;;; kaocha-runner.el ends here
