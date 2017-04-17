### Fuzzy matching

Helm's built-in fuzzy matcher is activated for some commands. Helm's
fuzzy matching is disabled by default. Currently these commands
support fuzzy matching:

- `helm-recentf`: set `helm-recentf-fuzzy-match` to `t`.
- `helm-mini`: set `helm-buffers-fuzzy-matching` and `helm-recentf-fuzzy-match` to `t`.
- `helm-buffers-list`: set `helm-buffers-fuzzy-matching` to `t`.
- `helm-find-files`: fuzzy matching enabled by default.
- `helm-locate`: set `helm-locate-fuzzy-match` to `t`.
- `helm-M-x`: set `helm-M-x-fuzzy-match` to `t`.
- `helm-semantic`: set `helm-semantic-fuzzy-match` to `t`.
- `helm-imenu`: set `helm-imenu-fuzzy-match` to `t`.
- `helm-apropos`: set `helm-apropos-fuzzy-match` to `t`.
- `helm-lisp-completion-at-point`: set `helm-lisp-fuzzy-completion` to `t`.

To globally enable fuzzy matching for `helm-mode`:
- set `helm-mode-fuzzy-match` to `t`.
- set `helm-completion-in-region-fuzzy-match` to `t`.

**IMPORTANT**: For faster fuzzy matching, set
`helm-candidate-number-limit` to 100 or less. Default is 100.

