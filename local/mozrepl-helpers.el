;; Define some helper functions to be able to drive Firefox directly
;; from Emacs using MozRepl.  To use these functions:
;;
;;   $ git clone http://github.com/osbert/mozrepl-helpers
;;
;; Edit your ~/emacs.d/init.el to add mozrepl-helpers to your load
;; path and then load the file:
;;
;;   (add-to-list 'load-path "~/mozrepl-helpers")
;;   (load "mozrepl-helpers.el")
;;
;; NOTE: You must also use moz.el for this to work:
;; http://github.com/bard/mozrepl/raw/master/chrome/content/moz.el
;;
;; You can then setup custom keybinds, here are some suggested defaults:
;;
;; (global-set-key (kbd "C-c C-b C-r") 'browser-reload)
;; (global-set-key (kbd "C-c C-b C-n") 'browser-scroll-down)
;; (global-set-key (kbd "C-c C-b C-p") 'browser-scroll-up)
;; (global-set-key (kbd "C-c C-b C-s") 'browser-search)

(require 'moz)

(defun mozrepl-eval (cmd)
  (comint-send-string (inferior-moz-process) cmd))

(defun browser-reload ()
  "Reload current tab."
  (interactive)
  (mozrepl-eval "BrowserReload()"))

;; NOTE: In Emacs you can specify 't' as ?T. However, all codes are
;; interpreted as lower-case, if you look at the implementation of
;; initKeyEvent the boolean flags control modifier keys like Shift,
;; Ctrl, and Alt.
(defun send-keycode-to-browser (keycode)
  "Send keycode to current browser window"
  (interactive "nKeycode: ")
  (mozrepl-eval 
   (format
    "var e = document.createEvent(\"KeyboardEvent\");e.initKeyEvent(\"keypress\", true, true, null, false, false, false, false, %d, 0);document.dispatchEvent(e)"
    keycode)))

;; For full list of key codes available
;; http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/events/nsIDOMKeyEvent.idl

(defun browser-scroll-down ()
  "Scroll current tab down"
  (interactive)
  (send-keycode-to-browser 32))

(defun browser-scroll-up ()
  "Scroll current tab up."
  (interactive)
  (send-keycode-to-browser 33))

(defun browser-search (search-terms)
  "Open a new tab searching for search-terms."
  (interactive "sSearch Term: ")
  (mozrepl-eval (format "BrowserOpenTab(); BrowserSearch.loadSearch(\"%s\");" search-terms)))

(defun browser-left-arrow ()
  (interactive)
  (send-keycode-to-browser 37))

(defun browser-right-arrow ()
  (interactive)
  (send-keycode-to-browser 39))

(defun browser-next-tab (n)
  (interactive "p")
  (when (> n 0)
    (mozrepl-eval "gBrowser.tabContainer.advanceSelectedTab(1, true);")
    (browser-next-tab (- n 1))))

(defun browser-previous-tab (n)
  (interactive "p")
  (when (> n 0)
    (mozrepl-eval "gBrowser.tabContainer.advanceSelectedTab(-1, true);")
    (browser-previous-tab (- n 1))))

(defun browser-close-tab ()
  (interactive)
  (mozrepl-eval "gBrowser.removeCurrentTab()"))

(defun browser-open-tab (url)
  (interactive "sURL: ")
  (mozrepl-eval (format "gBrowser.selectedTab = gBrowser.addTab('%s')" url)))

(defun browser-eval (cmd)
  "Eval JavaScript in the context of the current active tab."
  ;; NOTE: This does not escape double quotes correctly.
  (interactive "sCommand: ")
  (mozrepl-eval "repl.enter(content)")
  (mozrepl-eval cmd)
  (mozrepl-eval "repl.back()"))
