<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
    - [Before starting: set `HOME`](#before-starting-set-home)
- [git-gui--askpass](#git-gui--askpass)
- [Passphrase-less OpenSSH key](#passphrase-less-openssh-key)
- [OpenSSH passphrase caching, via ssh-agent](#openssh-passphrase-caching-via-ssh-agent)
- [Using SSH keys with PuTTY](#using-ssh-keys-with-putty)

<!-- markdown-toc end -->

# Introduction

Windows does not have the concept of a [PTY](https://en.wikipedia.org/wiki/Pseudo_terminal), so there is no way for Emacs to intercept password prompts from Git. You will get an error message like
```
bash: /dev/tty: No such device or address
error: failed to execute prompt script (exit code 1)
fatal: could not read Username for 'https://github.com': No error
```

NOTE: Git for Windows 1.x simply hangs indefinitely.

It's possible to work around this by configuring Git so that Magit doesn't
need to send it a password, there are several methods to do this.

The following assumes native Emacs and [Git for Windows][wgit] (version 2 or greater is highly recommended).

[wgit]: https://git-for-windows.github.io/

## Before starting: set `HOME`

If you haven't explicitly set the `HOME` environment variable, do so
now. Not doing this means git (and ssh) will use a different `HOME`
depending on whether or not they were called from Emacs. For https
remotes this will only have the effect of ignoring any `.gitconfig`
you may have set. But if OpenSSH can't find it's config files
(especially `known_hosts`), you won't be able to use it from magit at
all.

The easiest way is to run `setx HOME C:/path/to/home` from a `cmd.exe`
console. See
[What are PATH and other environment variables, and how can I set or use them?][win-env]
for more information.

Note that you should move your config files from whichever `HOME` you
didn't choose to the new common one.

[win-env]: https://superuser.com/questions/284342/what-are-path-and-other-environment-variables-and-how-can-i-set-or-use-them/284351#284351


# git-gui--askpass

If you access the remote over https, the `GIT_ASKPASS` environment
variable can be used to tell git to use a gui prompt for the password.
Add the following to your init file:

    (setenv "GIT_ASKPASS" "git-gui--askpass")

It will be quite annoying to enter your username and password every
time, so you should also use password caching: run `git config
--global credential.helper wincred` to enable it (requires Git for
Windows 1.8.1 or greater)

Pros:
- No additional installation needed: the "git-gui--askpass" program
comes builtin with git.

Cons:
- The GUI prompt doesn't mesh so nicely with Emacs

NOTE: In Git for Windows 1.x, if you press cancel at the prompt Magit will still hang.

**Troubleshooting**

Check that your remote is using an https url.

This should work from cmd.exe (assuming you have git.exe in `%PATH%`).
First run `set GIT_ASKPASS=git-gui--askpass` to simulate the Emacs
setup given above.

# Passphrase-less OpenSSH key

If git uses a passphrase-less SSH key, then there is no need to type
in anything. The procedure for installing keys depends on the remote, for
Github see https://help.github.com/articles/generating-ssh-keys/.

Pros:
- No additional installation needed: OpenSSH comes installed with Git
for Windows.
- Don't need to type anything after the 1 time setup.

Cons:
- It's not secure to use a passphrase-less key: read access to the
  private key file on your dev machine now gives write access to any
  remotes!

**Troubleshooting**

Check that your remote is using an SSH url.

Make sure you have explictly set `HOME`
([see above](#before-starting-set-home)).

Check that you can run `git push` from the command line without
entering any password/passphrase.

# OpenSSH passphrase caching, via ssh-agent

If an OpenSSH key is protected by a passphrase, it can be used with
Magit by caching the passphrase with ssh-agent. Install the
[ssh-agency] package to manage this for you.

Also add

    (setenv "SSH_ASKPASS" "git-gui--askpass")

to your init file to get a graphical prompt on the first where
you need to enter your passphrase (the alternative is ssh-agency
pops up a console cmd.exe style window).

Pros:
- No additional installation (apart from a small Emacs package)
needed: OpenSSH comes installed with Git for Windows.
- Don't need to type anything after entering the passphrase on
startup.
- Secure: your SSH keys cannot be used without entering the
passphrase.

Cons:
- One more package to install.

[ssh-agent]: https://help.github.com/articles/working-with-ssh-key-passphrases/#platform-windows
[ssh-agency]: https://github.com/magit/ssh-agency


Note: The `SSH_ASKPASS` setting does not work with Git for
Windows 1.x (see
https://stackoverflow.com/questions/10960269/git-ssh-askpass-on-windows).
*If you have a reference to where/when/how this was fixed in Git for
Windows 2.x please edit it in*.

**Troubleshooting**

Check that using a passphrase-less key works first.

Check the result of evaluating:

```elisp
(list ssh-agency-add-executable (file-executable-p ssh-agency-add-executable)
      ssh-agency-agent-executable (file-executable-p ssh-agency-agent-executable))
```

You should get something like
`("c:/Program Files (x86)/Git/bin/ssh-add.exe" t "c:/Program Files (x86)/Git/bin/ssh-agent.exe" t)`

If you get `nil` instead of `t`, customize `ssh-agency-add-executable`
and `ssh-agency-agent-executable` to the correct paths (this should
only be necessary if you installed ssh in a non-standard place).

If it still doesn't work, try running:
```elisp
(call-process-shell-command
  (concat "start \"ssh-add\" cmd /K" (shell-quote-argument ssh-agency-add-executable)))
```

This will open a console. If you didn't get prompted to input your
passphrase, try running the following commands at the console
(adjusting the `ssh-add.exe` paths if needed):

```
"c:/Program Files (x86)/Git/bin/ssh-add.exe" -l
"c:/Program Files (x86)/Git/bin/ssh-add.exe" path/to/your/ssh/key/
```

Customize `ssh-agency-keys` so `ssh-agency` will find your keys
automatically from now on.


# Using SSH keys with PuTTY

It's possible to use PuTTY to manage SSH keys instead of OpenSSH. This
is similar to using ssh-agent, the main difference is that PuTTY's
pagent (its ssh-agent equivalent) is GUI based, and is accessible from
an icon in the system tray.

1. Download plink.exe, pagent.exe, and puttygen.exe from
https://www.chiark.greenend.org.uk/~sgtatham/putty/download.html.
2. Set the `GIT_SSH` variable to the full path of plink.exe. On a Windows 10 system configured with MSys2 for Bash usage, `/C/Program\\ Files\\ \\(x86\\)/PuTTY/plink.exe` worked with the eLisp expression below. 
3. Generate (or import an existing OpenSSH) private key using puttygen.exe.
4. Install the public key on the remote (as for OpenSSH the exact
procedure here depends on the server).
5. Start pagent.exe and add the key.
6. 1st time connecting to a host only: execute `plink
   <user>@<hostname>` and answer yes to add the host's key to the list
   of known hosts.

See also: http://gitolite.com/gitolite/putty.html - general
instructions for using PuTTY with git.

Pros:
- Don't need to type anything after entering the passphrase on
startup.
- Secure: your SSH keys cannot be used without entering the
passphrase.

Cons:
- Need to install additional software.

**Troubleshooting**

- Check that your remote is using an SSH url.

- Check that `(getenv "GIT_SSH")` returns the full path to `plink.exe`, so you should get something like `plink: Release 0.64` when evaluating the code below:
```elisp
(shell-command-to-string (concat (getenv "GIT_SSH") " -V"))
```

- Check the output from running `plink <user>@<hostname>` from cmd.exe.
