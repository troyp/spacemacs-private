(require 'hydra)
(defhydradio hydra-tng ()
  (picard "_p_ Captain Jean Luc Picard:")
  (riker "_r_ Commander William Riker:")
  (data "_d_ Lieutenant Commander Data:")
  (worf "_w_ Worf:")
  (la-forge "_f_ Geordi La Forge:")
  (troi "_t_ Deanna Troi:")
  (dr-crusher "_c_ Doctor Beverly Crusher:")
  (phaser "_h_ Set phasers to " [stun kill]))

(defhydra hydra-tng (:foreign-keys run :hint nil)
  (concat (hydra--table hydra-tng/names 5 3
                        '("  % -30s %% -3`%s"
                          "%s %%`%s"))
          "\n\n")
  ("p" (hydra-tng/picard))
  ("r" (hydra-tng/riker))
  ("d" (hydra-tng/data))
  ("w" (hydra-tng/worf))
  ("f" (hydra-tng/la-forge))
  ("t" (hydra-tng/troi))
  ("c" (hydra-tng/dr-crusher))
  ("h" (hydra-tng/phaser))
  ("b" beam-down "beam down" :exit t)
  ("o" (hydra-reset-radios hydra-tng/names) "reset")
  ("q" nil "cancel"))

(defun beam-down ()
  (interactive)
  (message
   "Beaming down: %s."
   (mapconcat
    #'identity
    (delq nil
          (mapcar
           (lambda (p) (when (symbol-value p)
                    (substring (symbol-name p) 10)))
           '(hydra-tng/picard
             hydra-tng/riker
             hydra-tng/data
             hydra-tng/worf
             hydra-tng/la-forge
             hydra-tng/troi
             hydra-tng/dr-crusher)))
    ", and ")))

(global-set-key (kbd "C-c C-,") 'hydra-tng/body)
