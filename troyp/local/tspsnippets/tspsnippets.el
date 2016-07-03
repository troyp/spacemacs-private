;; *****************************
;; *                           *
;; * SNIPPET TEMPLATE COMMANDS *
;; *                           *
;; *****************************

;; TODO: Either (a) convert to yasnippet snippet, or
;;              (b) write snippet insertion infrastructure

(defun print-getopt-snippet (shorts longs helptext)
  (interactive "sShort Options (no spaces): \nsLong Options (sep. by commas): \nsHelp Text: ")
  (let ((startpoint (point)))
    (insert (concat
             "PARSED=$(getopt -n \"$0\" -o h" shorts ": --long \"help," longs  "\" -- \"$@\")
if [ $? -ne 0 ]; then echo \"getopt error\"; return 1; fi
eval set -- \"$PARSED\"
while true; do
    case \"$1\" in
        -h|--help)
cat <<EOF\n"
             helptext
             "
Options:
-h --help           display this help
EOF
            shift
            exit 0
;;
"
             (let* ((shortlist (string-to-list shorts))
                    (longlist  (split-string longs ","))
                    (ziplist   (cl-mapcar #'cons shortlist longlist)))
               (cl-loop for short in shortlist
                        for long in longlist
                        concat (concat "        -" (string short) "|--" long ")\n")
                        concat "             shift\n"
                        concat "             ;;\n"))
             "esac
done
"
             ))
    (indent-region startpoint (point))))
