1. [Installation](#installation)
2. [Configuration](#configuration)
3. [Usage](#usage)
4. [Conclusion](#conclusion)

Here are the basic instructions to get smartparens working.

<a name="installation" />
# Installation

If you don't have emacs 24, or don't have `package.el` installed, or you want to install smartparens manually, read the [[installation manual|installation]]. Otherwise proceed on with this section.

Smartparens is available in the `melpa` repository, so the installation with all the dependencies is very simple. By default, the `melpa` repository is not added to the `package-archives` list, so you have to do that manually if you haven't done so.

To do that, open up your `.emacs` or `init.el` and add the following line:

```scheme
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
```

then place your cursor at the end of the line and type `C-x C-e`. That will execute the line and add the repository to the archive. You should leave the line in your config so it is automatically loaded next time you start emacs.

Next, you need to fetch the package list. To do that, use `M-x list-packages`. This will open the package list. Use `C-s smartparens` to locate the smartparens entry. Simply hit enter or click the link with mouse. That will open the window where you can install the package. Everything will download and compile automatically.

Then just add

```scheme
(package-initialize)
(smartparens-global-mode t)
```

to your `.emacs` or `init.el`, select the two lines and execute `M-x eval-region`. Smartparens will now automatically activate in all new buffers.

You should also make sure that you disable other similar packages while using smartparens.  `skeleton-mode` is known to cause incompatibilities, and other modes listed [here](https://github.com/Fuco1/smartparens/wiki#what-is-this-package-about) should also be disabled---or, if you for some reason want to use them, carefully configured to not cause problems.

**Tip:** since smartparens provide lots of customizability, it might be a good idea to create a separate file for smartparens configuration. Just create a new file called for example `init-smartparens.el` and place a load clause in your `.emacs` or `init.el`

```scheme
(load "init-smartparens")
```

<a name="configuration" />
# Configuration

Smartparens comes with sensible [[default configuration]], so most of the time you will only be adding new features and not configuring something that should already be done for you.  This default configuration is however optional and you have to explicitly load it by calling `(require 'smartparens-config)` somewhere in your initialization file.  See the link above for more information on default configuration.

You should get familiar with the customize facility in emacs. Many switches and tweaks in smartparens are available through this facility. You can `M-x customize-group smartparens` to list all the available options. If you already know the name of the option, you can load the single option by `M-x customize-variable name-of-variable.` Remember that all smartparens related functions and variables start with prefix `sp-`. This will help you to filter through all the available options.

To add aditional pairs, change permissions and add your own custom functions, you can check the [[example configuration]]. From there you can copy and modify the parts that interest you.

<a name="usage" />
# Usage

The first basic feature is autopairing of "pairable" characters like `(` `[` `{` `"` etc. When you type some of those, the closing pair is simply inserted for you and the point is placed in between them. You can [[add new pairs|Pair management]] specific to major modes you use.

Another feature is [[region wrapping|wrapping]]. To wrap a region in a pair, simply select the region and type the *opening* pair and the region will be wrapped from both sides. In html and other modes, you can wrap with html tags like `<span>`. Simply select a region and type `<`. Smartparens will enter a special mode where you'll enter the name of the tag and possible html arguments (like `class="list"` etc.).

Lastly, smartparens expand the default navigation facilities of `next-sexp`, `up-list` etc. The complete list of these functions with descriptions can be found in the article about [[working with expressions]]. You should definitely get familiar with these, since they speed up the work in the buffer a lot. These functions are not bound to any keys by default, so you'll need to do that yourself. The proposed defaults are in the aforementioned article.

To see the more common features in action, you can also watch this [youtube presentation](http://www.youtube.com/watch?v=ykjRUr7FgoI&list=PLP6Xwp2WTft7rAMgVPOTI2OE_PQlKGPy7&feature=plpp_play_all).

<a name="conclusion" />
# Conclusion

Smartparens make your life with pairs easier often without you even realizing it. After you get familiar with this package, have a look at the [advanced articles](home#wiki-are-you-experienced-emacs-user?) about all the additional features smartparens provide. There's also a collection of [[tips and tricks]] from the users that will make you appreciate SP even more :)
