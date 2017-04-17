### Why is the Emacsclient used in the first place?

Magit uses the Emacsclient when editing commit messages.  More
precisely Magit calls `git commit` without any other arguments,
effectively telling Git to "create a commit message template, and
then open that in the editor specified by the environment variable
`$GIT_EDITOR`".  The `$GIT_EDITOR` can be any reasonable editor, e.g.
`emacs` but Magit uses the `with-editor` library to make sure an
`emacsclient` executable is used.  In other words Magit makes sure
"Git knows how to call home".

The alternative would be to prepare a commit message and then provide
that message to Git upfront, as in `git commit -m "the message"`.
This has the considerable drawback that Git cannot prepare the commit
message, for the user to edit.  The user then always has to write the
message from scratch.  This would be especially annoying when amending
to an existing commit, or when merging.

### Why is it problematic to use the Emacsclient?

Magit (With-Editor actually) has to determine an Emacsclient
executable which is compatible with the running Emacs instance, and
that is problematic for three reasons.

1. On some platforms installing Emacs does not cause its executables
   to be placed on `$PATH`.  We work around this by also looking in
   some other directories likely to contain the executable.

2. We cannot just use the first executable found, instead we have to
   determine the versions of the found executables and then pick the
   one whose version matches that of the running Emacs instance as
   closely as possible.

   In the past we did not do so properly; now that we do, there should
   be no more problems.

3. The Emacsclient cannot be used over Tramp.  Well theoretically it
   could, but that would be too complicated to setup and also
   insecure.

   To address this With-Editor implements a "sleeping editor" which is
   a poor man's `emacsclient`.  While the `emacsclient` communicates
   with `emacs` using a socket, the "sleeping editor" does so on its
   standard output, where a special process filter is listening.

   This works over Tramp without any further setup.  And it can also
   be used locally, which is useful if we should ever fail to find a
   suitable Emacsclient executable.

### Still doesn't work for me!

See [Configuring With-Editor](https://magit.vc/manual/with-editor/Configuring-With_002dEditor.html)
and [Debugging With-Editor](https://magit.vc/manual/with-editor/Debugging.html).
