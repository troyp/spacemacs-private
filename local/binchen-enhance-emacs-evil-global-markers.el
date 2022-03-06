;; ╭──────────────────────────────────────────────────╮
;; │                                                  │
;; │  binchen.org: enhance emacs evil global markers  │
;; │                                                  │
;; ╰──────────────────────────────────────────────────╯

;; http://blog.binchen.org/posts/enhance-emacs-evil-global-markers/

(defvar evil-global-markers-history nil)
(defun my-forward-line (lnum)
  "Forward LNUM lines."
  (setq lnum (string-to-number lnum))
  (when (and lnum (> lnum 0))
    (goto-char (point-min))
    (forward-line (1- lnum))))

(defadvice evil-set-marker (before evil-set-marker-before-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (pos (or (nth 1 args) (point))))
    ;; only rememeber global markers
    (when (and (>= c ?A) (<= c ?Z) buffer-file-name)
      (setq evil-global-markers-history
            (delq nil
                  (mapcar `(lambda (e)
                             (unless (string-match (format "^%s@" (char-to-string ,c)) e)
                               e))
                          evil-global-markers-history)))
      (setq evil-global-markers-history
            (add-to-list 'evil-global-markers-history
                         (format "%s@%s:%d:%s"
                                 (char-to-string c)
                                 (file-truename buffer-file-name)
                                 (line-number-at-pos pos)
                                 (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                              (line-end-position)))))))))

(defadvice evil-goto-mark-line (around evil-goto-mark-line-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (orig-pos (point)))

    (condition-case nil
        ad-do-it
      (error (progn
               (when (and (eq orig-pos (point)) evil-global-markers-history)
                 (let* ((markers evil-global-markers-history)
                        (i 0)
                        m
                        file
                        found)
                   (while (and (not found) (< i (length markers)))
                     (setq m (nth i markers))
                     (when (string-match (format "\\`%s@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'"
                                                 (char-to-string c))
                                         m)
                       (setq file (match-string-no-properties 1 m))
                       (setq found (match-string-no-properties 2 m)))
                     (setq i (1+ i)))
                   (when file
                     (find-file file)
                     (my-forward-line found)))))))))

(defun counsel-evil-goto-global-marker ()
  "Goto global evil marker."
  (interactive)
  (unless (featurep 'ivy) (require 'ivy))
  (ivy-read "Goto global evil marker"
            evil-global-markers-history
            :action (lambda (m)
                      (when (string-match "\\`[A-Z]@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" m)
                        (let* ((file (match-string-no-properties 1 m))
                               (linenum (match-string-no-properties 2 m)))
                          (find-file file)
                          (my-forward-line linenum))))))
