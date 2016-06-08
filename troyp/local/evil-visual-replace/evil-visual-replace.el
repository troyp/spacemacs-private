;;; evil-visual-replace.el --- search/replace commands for evil visual state, inc. blocks

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: search replace regexp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides versions of the emacs query-replace (M-%) and replace-regexp (C-M-%)
;; commands which work for evil-mode visual-state, including visual blocks.
;; The native emacs versions don't understand evil blocks, and treat them as
;; normal regions.
;;
;; Note that these commands currently only support visual state, so they should
;; not be used to globally replace the native commands. Bind them in
;; evil-visual-state-map.
;;
;; Install:
;;
;; (evil-virep-visual-bindings)

;;; Code:

(require 'evil)

(defun evil-virep-visual-bindings ()
  (interactive)
  (define-key evil-visual-state-map (kbd "M-%") 'evil-virep-query-replace)
  (define-key evil-visual-state-map (kbd "C-M-%") 'evil-virep-replace-regexp)
  )

;;;###autoload
(evil-define-operator evil-virep-query-replace
  (start end type fromstr tostr  &optional delimited backward)
  "An interactive function which acts on the evil visual region.
Replace FROMSTR with TOSTR from START to END with CHAR.
If DELIMITED is non-nil (or a prefix argument is given interactively), only matches surrounded by word boundaries are replaced.
If BACKWARD is non-nil (or a negative prefix argument is given interactively), the replacement proceeds backward.
This operator respects visual-block selections. For non-block visual state operations, it is identical to `query-replace'.
For non-visual-state replacements, use `query-replace'."
  :motion evil-forward-char
  (interactive
   (let ((selection (evil-visual-range))
         (args (query-replace-read-args
                (concat
                 "Query replace"
                 (if current-prefix-arg
                     (let (arg (prefix-numeric-value current-prefix-arg))
                       (cond
                        ((< arg 0) "backward")
                        (otherwise "word"))
                       (if (eq current-prefix-arg '-) " backward" " word"))
                   "")
                 (if (and transient-mark-mode mark-active) " in region" ""))
                nil)))
     (list (nth 0 selection)
           (nth 1 selection)
           (nth 2 selection)
           (nth 0 args)
           (nth 1 args)
           (nth 2 args)
           (nth 3 args))))
  (when fromstr
    (if (eq type 'block)
        (save-excursion
          (defun do-replace (begcol endcol fromstr tostr)
            (let* ((maxcol (evil-column (line-end-position)))
                   (endcol (min endcol maxcol)))
              (unless (> begcol maxcol)
                (let ((begpos (evil-move-to-column begcol))
                      (endpos (evil-move-to-column endcol)))
                  (perform-replace fromstr tostr
                                   t nil delimited nil nil
                                   begpos endpos backward)))))
          (evil-apply-on-rectangle
           #'do-replace start end fromstr tostr))
      :else
      (perform-replace fromstr tostr
                       t nil delimited nil nil
                       start end backward))))

;;;###autoload
(evil-define-operator evil-virep-replace-regexp
    (start end type regexp tostr  &optional delimited backward)
    "An interactive function which acts on the evil visual region.
Replace REGEXP with TOSTR from START to END with CHAR.
If DELIMITED is non-nil (or a prefix argument is given interactively), only matches surrounded by word boundaries are replaced.
If BACKWARD is non-nil (or a negative prefix argument is given interactively), the replacement proceeds backward.
This operator respects visual-block selections. For non-block visual state operations, it is identical to `replace-regexp'.
For non-visual-state replacements, use `replace-regexp'."
    :motion evil-forward-char
    (interactive
     (let ((selection (evil-visual-range))
           (args (query-replace-read-args
                  (concat
                   "Query replace"
                   (if current-prefix-arg
                       (let (arg (prefix-numeric-value current-prefix-arg))
                         (cond
                          ((< arg 0) "backward")
                          (otherwise "word"))
                         (if (eq current-prefix-arg '-) " backward" " word"))
                     "")
                   (if (and transient-mark-mode mark-active) " in region" ""))
                  nil)))
       (list (nth 0 selection)
             (nth 1 selection)
             (nth 2 selection)
             (nth 0 args)
             (nth 1 args)
             (nth 2 args)
             (nth 3 args))))
    (when regexp
      (if (eq type 'block)
          (save-excursion
            (defun do-replace (begcol endcol regexp tostr)
              (let* ((maxcol (evil-column (line-end-position)))
                     (endcol (min endcol maxcol)))
                (unless (> begcol maxcol)
                  (let ((begpos (evil-move-to-column begcol))
                        (endpos (evil-move-to-column endcol)))
                    (perform-replace regexp tostr
                                     t t delimited nil nil
                                     begpos endpos backward)))))
            (evil-apply-on-rectangle
             #'do-replace start end regexp tostr))
        :else
        (perform-replace regexp tostr
                         t t delimited nil nil
                         start end backward))))

(provide 'evil-visual-replace)

;;; evil-visual-replace.el ends here
