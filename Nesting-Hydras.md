## Nesting Hydras

As you can see in [[Basics]], a hydra named `hydra-zoom` generates a
function named `hydra-zoom/body`. This function can immediately be
used in another hydra, just like any other function.  The only thing
that you have to remember is to add `:exit t` to the appropriate head,
since only one hydra can be active at a time.

See [hydra-ox.el](https://github.com/abo-abo/hydra/blob/master/hydra-ox.el) for an example
implementation of a nested hydra.

## Order of calls to `:post` and `:body-pre` functions
Consider these two hydras:
```emacs-lisp
(defhydra test-a (:post (message "post a"))
  "a"
  ("q" test-b/body "to b" :exit t))

(defhydra test-b (:body-pre (message "pre b"))
  "b"
  ("q" nil "exit" :exit t))
```
Hitting key `q` from hydra `test-a` prints these messages:
```
post a
pre-b
```

That is:

- first, the old hydra exits.
- second, `test-b/body` is called.

## Visiting other hydras temporarily

Here's an example of visiting another hydra and going back to the
original one on exit.  Starting from `hydra-a`, pressing
<kbd>bcqq</kbd> will take you to "b", "c", "b", "a". And after one
more <kbd>q</kbd>, you'll exit.


```elisp
(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

(defhydra hydra-a (:color teal)
  "a"
  ("b" (progn
         (hydra-b/body)
         (hydra-push '(hydra-a/body)))
       "visit hydra-b")
  ("c" (progn
         (hydra-c/body)
         (hydra-push '(hydra-a/body)))
       "visit hydra-c")
  ("i" (message "I am a") :exit nil)
  ("q" hydra-pop "exit"))

(defhydra hydra-b (:color teal)
  "b"
  ("a" (progn
         (hydra-a/body)
         (hydra-push '(hydra-b/body)))
       "visit hydra-a")
  ("c" (progn
         (hydra-c/body)
         (hydra-push '(hydra-b/body)))
       "visit hydra-c")
  ("i" (message "I am b") :exit nil)
  ("q" hydra-pop "exit"))

(defhydra hydra-c (:color teal)
  "c"
  ("a" (progn
         (hydra-a/body)
         (hydra-push '(hydra-c/body)))
       "visit hydra-a")
  ("b" (progn
         (hydra-b/body)
         (hydra-push '(hydra-c/body)))
       "visit hydra-b")
  ("i" (message "I am c") :exit nil)
  ("q" hydra-pop "exit"))
```
