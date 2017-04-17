# Idris

- [`idris`](http://www.idris-lang.org/)
- [`idris-mode`](https://github.com/idris-hackers/idris-mode)

```lisp
(defhydra hydra-idris (:color blue :hint nil)
  "
                                                                    ╭───────────┐
                         Edit                                       │   IDRIS   │
 ╭──────────────────────────────────────────────────────────────────┴───────────╯
  [_l_] load            [_cd_] case dwim      [_bn_]  browse namespace
  Docs:^^               [_cs_] case split
  [_t_] type at point   [_a_] proof search
  [_dd_] doc at point   [_e_] add missing
  [_da_] apropos        [_l_] make lemma
  [_dt_] type search    [_s_] add clause
  General:^^            [_w_] make with block
  [_z_] pop to repl     [_ch_] case from hole
  [_SPC_] show menu     [_r_] refine
  [_C-SPC_] compl read  [_dc_] delete forward
                    ^^  [_dn_] next error
                    ^^  [_dp_] previous error
 --------------------------------------------------------------------------------


  ("<ESC>" nil "quit")
  ("q" nil "quit")
  ;; load file
  ("l" idris-load-file)
  ;;("n" idris-load-forward-line :color red)
  ;;("p" idris-load-backward-line :color red)

  ;; documentation lookup
  ("t" idris-type-at-point)
  ("dd" idris-docs-at-point)
  ("da" idris-apropos)
  ("dt" idris-type-search)


  ;; editing code
  ("cd" idris-case-dwim)
  ("cs" idris-case-split)
  ("e" idris-add-missing)
  ("l" idris-make-lemma)
  ("s" idris-add-clause)
  ("w" idris-make-with-block)
  ("a" idris-proof-search)
  ("r" idris-refine)
  ("ch" idris-make-cases-from-hole)
  ("RET" idris-newline-and-indent)

  ("dc" idris-delete-forward-char)
  ("dn" idris-next-error)
  ("dp" idris-previous-error)

  ;; general
  ("z" idris-pop-to-repl)
  ("SPC" prop-menu-show-menu)
  ("C-SPC" prop-menu-by-completing-read)

  ;; active terms
  ("mn" idris-normalize-term)
  ("mi" idris-show-term-implicits)
  ("mh" idris-hide-term-implicits)
  ("mc" idris-show-core-term)

  ;; misc
  ("bn" idris-browse-namespace)

  ;; working with a current package
  ("bb" idris-ipkg-build)
  ("bc" idris-ipkg-clean)
  ("bi" idris-ipkg-install)

  ;; editing package
  ("f" idris-ipkg-insert-field)

  ;; open package
  ("bp" idris-open-package-file))
```
