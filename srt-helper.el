;;; srt-helper.el --- Helper for adjusting SubRip subtitle timings

;; Author: death <github.com/death>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: entertainment
;; URL: http://github.com/death/srt-helper

;; This file is not part of GNU Emacs.

;; Copyright (c) 2015 death

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; To use this package: require it, find an SRT file, execute
;; `srt-helper', choose a time unit, and use the +/- keys to adjust
;; the timings.

;;; Code:

(defvar srt-helper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?-] 'srt-helper-sub)
    (define-key map [?=] 'srt-helper-add)
    (define-key map [?+] 'srt-helper-add)
    (define-key map [?q] 'kill-buffer-and-window)
    map))

(defmacro srt-helper-defunits (&rest spec)
  `(progn
     ,@(loop for (setter key delta title-format) in spec
             collect `(progn
                        (define-key srt-helper-mode-map [,key] ',setter)
                        (defun ,setter ()
                          (interactive)
                          (setq-local srt-helper-delta ,delta)
                          (srt-helper-refresh))))
     (defun srt-helper-usage-text ()
       (concat ,@(loop for (setter key delta title-format) in spec
                       collect `(format ,title-format
                                        (if (= srt-helper-delta ,delta)
                                            ,(upcase key)
                                          ,key))
                       collect " ")
               "[-] subtract" " "
               "[+|=] add" "\n"))))

(srt-helper-defunits
 (srt-helper-second ?s 1000 "[%c]econd")
 (srt-helper-halfsecond ?h 500 "[%c]alfsecond")
 (srt-helper-minute ?m 60000 "[%c]inute"))

(define-derived-mode srt-helper-mode fundamental-mode "SRT Helper"
  "Major mode for SRT Helper popup."
  (setq buffer-read-only t)
  (setq-local scroll-margin 0)
  (setq-local srt-helper-delta 1000))

;;;###autoload
(defun srt-helper ()
  (interactive)
  (let ((srt-buffer (current-buffer))
        (helper-buffer (get-buffer-create "*SRT Helper*")))
    (srt-helper-display helper-buffer)
    (with-current-buffer helper-buffer
      (set (make-local-variable 'srt-helper-origin) srt-buffer))
    (srt-helper-refresh)
    (fit-window-to-buffer)))

(defun srt-helper-display (buffer)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buffer)
  (srt-helper-mode))

(defun srt-helper-refresh ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (insert (srt-helper-usage-text)))
    (set-buffer-modified-p nil)))

(defun srt-helper-add ()
  (interactive)
  (srt-helper-transform srt-helper-delta))

(defun srt-helper-sub ()
  (interactive)
  (srt-helper-transform (- srt-helper-delta)))

(defvar srt-helper-range-regexp
  "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\),\\([0-9]\\{3\\}\\) --> \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\),\\([0-9]\\{3\\}\\)\n")

(defun srt-helper-transform (delta)
  (with-current-buffer srt-helper-origin
    (save-excursion
      (goto-char 0)
      (while (not (eobp))
        (let ((line (thing-at-point 'line)))
          (if (string-match srt-helper-range-regexp line)
              (let ((range (srt-helper-parse)))
                (when range
                  (insert (format "%s --> %s\n"
                                  (srt-helper-format (+ (first range) delta))
                                  (srt-helper-format (+ (second range) delta))))
                  (let ((beg (point))
                        (end nil))
                    (forward-line)
                    (setf end (point))
                    (delete-region beg end))))
            (forward-line)))))))

(defun srt-helper-parse ()
  (list (+ (* (string-to-number (match-string 1 line)) 1000 60 60)
           (* (string-to-number (match-string 2 line)) 1000 60)
           (* (string-to-number (match-string 3 line)) 1000)
           (* (string-to-number (match-string 4 line))))
        (+ (* (string-to-number (match-string 5 line)) 1000 60 60)
           (* (string-to-number (match-string 6 line)) 1000 60)
           (* (string-to-number (match-string 7 line)) 1000)
           (* (string-to-number (match-string 8 line))))))

(defun srt-helper-format (range)
  (let (h m s ms)
    (setq ms (mod range 1000))
    (setq range (floor range 1000))
    (setq s (mod range 60))
    (setq range (floor range 60))
    (setq m (mod range 60))
    (setq range (floor range 60))
    (setq h range)
    (format "%02d:%02d:%02d,%03d" h m s ms)))

(provide 'srt-helper)

;;; srt-helper.el ends here
