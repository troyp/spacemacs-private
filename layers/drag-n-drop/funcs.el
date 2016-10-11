;; ,---------------------,
;; | Drag-n-drop Buffers |
;; '---------------------'

;; from https://tsdh.wordpress.com/2015/03/03/swapping-emacs-windows-using-dragndrop/
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))

(defun drag-n-drop-smart-dnd-set-to-drop-filenames ()
  "Configure `smart-dnd-setup' to drop file names."
  (interactive)
  (smart-dnd-setup '((".*" . "%f"))))

(defun drag-n-drop-smart-dnd-set-to-drop-html-links ()
  "Configure `smart-dnd-setup' to drop html links.
Inserts and <img>, <link> or <script> tag as appropriate, or falls back to an
<a> tag."
  (interactive)
  (smart-dnd-setup
   '(("\\.\\(png\\|gif\\)\\'" . "<img src=\"%R\">\n")
     ("\\.jpe?g\\'" . "<img src=\"%R\">\n")
     ("\\.css\\'"   . "<link rel=\"stylesheet\" type=\"text/css\" href=\"%R\">\n" )
     ("\\.js\\'"    . "<script type=\"text/javascript\" src=\"%R\"></script>\n" )
     (".*" . "<a href=\"%R\">%f</a>\n")
     )))
