

(require 'dash)
(require 'f)
(require 's)

(defvar asciihd-figlet-font-search-path '("/usr/share/figlet" "/usr/local/share/figlet"))
(defvar asciihd-figlet-font-dirs
  (-filter 'f-dir?
           asciihd-figlet-font-search-path))

(defvar asciihd-figlet-executable-search-path '("/usr/bin/figlet" "/usr/local/bin/figlet"))
(defvar asciihd-figlet-executable
  (-find 'f-executable?
         asciihd-figlet-executable-search-path))

(defun asciihd-figlet-list-fonts ()
  (defun .flf? (f)
    (equal (f-ext f) "flf"))
  (cl-loop
   for dir in asciihd-figlet-font-dirs
   append (f-files dir #'.flf? t)))

(defvar asciihd-figlet-font-list (asciihd-figlet-list-fonts))

(defvar asciihd-figlet-font-names (-map #'f-filename
                                        asciihd-figlet-font-list))

(defun asciihd-figlet-font-path (fontname)
  (defun has-fontname? (p)
    (s-ends-with? (concat "/" fontname ".flf")
                  p))
  (-first #'has-fontname?
          asciihd-figlet-font-list))

(defun asciihd-figlet-font-directory (fontname)
  (f-dirname (asciihd-figlet-font-path fontname)))

(cl-defun asciihd-sfiglet
    (s
     &optional (fontname "standard")
     &key (no-trailing-whitespace t))
  (let* ((fontdir  (asciihd-figlet-font-directory fontname))
         (rawstring (shell-command-to-string
                     (concat asciihd-figlet-executable
                             " -d " fontdir " "
                             " -f " fontname " "
                             "\"" s "\""))))
    (if :no-trailing-whitespace
        (mls-trim-line-ends rawstring)
      rawstring)))

(cl-defun asciihd-ssfiglet
    (s
     &optional (fontname "standard")
     &key (no-trailing-whitespace t))
  (s-lines (asciihd-sfiglet s fontname :no-trailing-whitespace no-trailing-whitespace)))

(cl-defun asciihd-figlet
    (s
     &optional (fontname "standard")
     &key (no-trailing-whitespace t))
  (interactive "sString: \nsFiglet Font Name [default \"standard\"]: ")
  (insert (asciihd-sfiglet s fontname :no-trailing-whitespace no-trailing-whitespace)))
