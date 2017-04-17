### Autoresize

To re-size the completion window based on number of candidates:

    (helm-autoresize-mode 1)

Adjust minimum and maximum height of completion window using:

- `helm-autoresize-max-height`
- `helm-autoresize-min-height`

40 is the default value of `helm-autoresize-max-height`, which sets the completion window height to 40% of the fame height. `helm-autoresize-min-height` specifies the minimum height that the completion window cannot shrink to.

For a fixed window size, set `helm-autoresize-min-height` equal to `helm-autoresize-max-height`.

