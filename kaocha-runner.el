;;; kaocha-runner.el --- A package for running Kaocha tests via CIDER.

;; Copyright (C) 2019 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.4.0") (cider "0.21.0") (edn "1.1.2"))

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
(require 'edn)
(require 's)

(defun kaocha--eval-clojure-code-sync (code)
  (cider-nrepl-sync-request:eval
   code
   (cider-current-repl nil 'ensure)
   (cider-current-ns)))

(defun kaocha--eval-clojure-code (code callback)
  (cider-nrepl-request:eval
   code
   callback
   (cider-current-ns)
   nil nil nil
   (cider-current-repl nil 'ensure)))

(setq kaocha--out-buffer "*kaocha-output*")
(setq kaocha--err-buffer "*kaocha-error*")

(defun kaocha--clear-buffer (buffer)
  (get-buffer-create buffer)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))))

(defun kaocha--colorize ()
  (save-excursion
    (goto-char (point-min))
    (insert "[m")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun kaocha--insert (buffer s)
  (with-current-buffer buffer
    (insert s)
    (kaocha--colorize)))

(defmacro with-kaocha-window (buffer original-buffer &rest body)
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

(defun kaocha--fit-window-snuggly (min-height max-height)
  (window-resize nil (- (max min-height
                             (min max-height
                                  (- (line-number-at-pos (point-max))
                                     (line-number-at-pos (point)))))
                        (window-height))))

(defun kaocha--recenter-top ()
  (recenter (min (max 0 scroll-margin)
                 (truncate (/ (window-body-height) 4.0)))))

(defun kaocha--num-warnings ()
  (s-count-matches "WARNING:"
                   (with-current-buffer kaocha--err-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))))

(defun kaocha--show-report (value current-ns)
  (when-let* ((result (edn-read (s-chop-prefix "#:kaocha.result" value))))
    (let* ((tests (gethash :count result))
           (pass (gethash :pass result))
           (fail (gethash :fail result))
           (err (gethash :error result))
           (warnings (kaocha--num-warnings))
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

(setq kaocha--fail-re "\\(FAIL\\|ERROR\\)")

(defun kaocha--show-details-window (original-buffer min-height)
  (with-kaocha-window kaocha--out-buffer original-buffer
    (visual-line-mode 1)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward kaocha--fail-re nil t))
    (end-of-line)
    (kaocha--fit-window-snuggly min-height 16)
    (kaocha--recenter-top)))

(defcustom kaocha--repl-invocation-template
  "(do (require 'kaocha.repl) %s)"
  "The invocation sent to the REPL to run kaocha tests, with the actual run replaced by %s.")

(defun kaocha--run-tests (&optional run-all? background?)
  (interactive)
  (kaocha--clear-buffer kaocha--out-buffer)
  (kaocha--clear-buffer kaocha--err-buffer)
  (kaocha--eval-clojure-code
   (format kaocha--repl-invocation-template (if run-all?
                                                "(kaocha.repl/run-all)"
                                              "(kaocha.repl/run)"))
   (lexical-let ((current-ns (cider-current-ns))
                 (original-buffer (current-buffer))
                 (done? nil)
                 (any-errors? nil)
                 (shown-details? nil)
                 (the-value nil))
     (if run-all?
         (kaocha--show-details-window original-buffer 12)
       (unless background?
         (message "[%s] Running tests ..." current-ns)))
     (lambda (response)
       (nrepl-dbind-response response (content-type content-transfer-encoding body
                                                    value ns out err status id)
         (when out
           (kaocha--insert kaocha--out-buffer out)
           (when (let ((case-fold-search nil))
                   (string-match-p kaocha--fail-re out))
             (setq any-errors? t)))
         (when err
           (kaocha--insert kaocha--err-buffer err))
         (when value
           (setq the-value value))
         (when (and status (member "done" status))
           (setq done? t))
         (when (and done? the-value)
           (kaocha--show-report the-value current-ns))
         (when (and done? any-errors? (not shown-details?))
           (setq shown-details? t)
           (kaocha--show-details-window original-buffer 4)))))))

(defun kaocha-hide-windows ()
  (interactive)
  (when (get-buffer kaocha--out-buffer)
    (kill-buffer kaocha--out-buffer))
  (when (get-buffer kaocha--err-buffer)
    (kill-buffer kaocha--err-buffer)))

(defun kaocha-run-tests (&optional run-all?)
  "Run tests in the current namespace. With a prefix argument, run all tests."
  (interactive "P")
  (kaocha-hide-windows)
  (kaocha--run-tests run-all?))

(defun kaocha-show-warnings (&optional switch-to-buffer?)
  (interactive "P")
  (if switch-to-buffer?
      (switch-to-buffer-other-window kaocha--err-buffer)
    (message "%s"
             (s-trim
              (with-current-buffer kaocha--err-buffer
                (buffer-substring (point-min) (point-max)))))))

(provide 'kaocha)
;;; kaocha-runner.el ends here
