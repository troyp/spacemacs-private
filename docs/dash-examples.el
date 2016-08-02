
(-partition-by-header (-rpartial 'mod 4) (range from 3 to 25))
;; ((3 4 5 6) (7 8 9 10) (11 12 13 14) (15 16 17 18) (19 20 21 22) (23 24 25))

(funcall (-compose (-partial '+ 1) (-partial '* 10)) 3)
;; 31

(funcall (-juxt '-sum 'length) '(3 4 5 5 10))
;; (27 5)

(cl-flet
    ((mean
      (list)
      (apply '/ (funcall (-juxt '-sum 'length) list))))
  (mean '(3.0 4.0 5.0 5.0 10.0)))
;; 5.4

