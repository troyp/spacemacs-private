;;; sdic-mode �Ѥ�����
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(autoload 'sdic-describe-word "sdic" "��ñ��ΰ�̣��Ĵ�٤�" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "��������ΰ��֤α�ñ��ΰ�̣��Ĵ�٤�" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)
