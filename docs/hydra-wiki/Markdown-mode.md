A hydra for some markdown-mode related commands. At the moment I am looking for a nicer head layout but you can use it right now.

    (defhydra dh-hydra-markdown-mode (:hint nil)
      "
    Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
    
    Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
    
    Lists             C-c C-x    _m_: insert item   
    
    Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
    
    Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
     
    "
    
    
      ("s" markdown-insert-bold)
      ("e" markdown-insert-italic)
      ("b" markdown-insert-blockquote :color blue)
      ("p" markdown-insert-pre :color blue)
      ("c" markdown-insert-code)
    
      ("h" markdown-insert-header-dwim) 
      ("1" markdown-insert-header-atx-1)
      ("2" markdown-insert-header-atx-2)
      ("3" markdown-insert-header-atx-3)
      ("4" markdown-insert-header-atx-4)
    
      ("m" markdown-insert-list-item)
    
      ("l" markdown-promote)
      ("r" markdown-demote)
      ("d" markdown-move-down)
      ("u" markdown-move-up)  
    
      ("L" markdown-insert-link :color blue)
      ("U" markdown-insert-uri :color blue)
      ("F" markdown-insert-footnote :color blue)
      ("W" markdown-insert-wiki-link :color blue)
      ("R" markdown-insert-reference-link-dwim :color blue) 
    )
    
    
    (global-set-key [f9] 'dh-hydra-markdown-mode/body)