
My Emacs Configuration
======================

This repository is intended to be cloned to ~/.emacs.d.

It was originally based on Phil Hagelberg's Emacs Starter Kit. As I've
now grown beyond passing interest and casual use of Emacs and made it
my primary tool for writing plain text and code, I leveled up my
configuratino repository.

Installation
============

This repository assumes GNU Emacs 24. I made no efforts to ensure this
configuration works on older or other Emacsen. I assume you know how to install
GNU Emacs 24 for your platform. If not, this probably isn't the configuration
for you(*).

Simply clone the repository to ~/.emacs.d. Fire up Emacs. It should
handle everything for you, including setting your theme to the best
one ever, ir-black :).

(*) - Of course, no one's Emacs configuration is suitable for everyone else.

Files
=====

COPYING - The GNU General Public License. See Credits and License.
README.md - This file.
appearance.el - Configuration related to look and feel
behavior.el - Configuration related to affecting behavior
custom.el - The `custom` file, must exist, but is .gitignored otherwise
elpa - Automatically created by packages.el
env.el - Affects ENV variables and the like.
helpers.el - Various defuns to help elsewhere.
init.el - Kicks it all off.
keybindings.el - My favorite keybindings in one low-fat place.
modules - Customization to modes, modules.
packages.el - Manages packages from ELPA (uses Marmalade, see __Packages__)
themes - My themes, using Emacs 24's theme system.
vendor - elisp code that isn't packaged, e.g. from emacswiki.

Packages
========

I chose to use the built-in Emacs 24 package system exclusively. I
tried [el-get](https://github.com/dimitri/el-get), but I couldn't get
everything to work properly. I use Marmalade instead of the Tromey
repository primarily because that is where Emacs Starter Kit packages
are, and it seems to have more current packages.

Theme
=====

In the themes directory is `ir-black`, which is my favorite theme for
any editor. I made some customizations from the original:

* [Jon-Michael Deldin](https://github.com/jmdeldin/ir-black-theme.el)

Burke Libbey also has an `ir-black` color-theme. It is very similar,
and should work with `color-theme` just fine.

* [Burke Libbey](https://github.com/burke/color-theme-ir-black)

Credits and License
===================

Author:: Joshua Timberman <opensource@housepub.org>

I have used and borrowed code from other people's Emacs repositories,
including:

Phil Hagelberg,
[Emacs Starter Kit](https://github.com/technomancy/emacs-starter-kit)

Jim Myhrberg,
[.emacs.d](https://github.com/jimeh/.emacs.d)

Steven Danna,
[emacs-config](https://github.com/stevendanna/emacs-config)

Jon-Michael Deldin,
[ir-black-theme](https://github.com/jmdeldin/ir-black-theme.el)

Some of this code is specifically licensed under the GNU General
Public License. Some of it is not. I generally prefer more liberal
licenses such as the Apache 2.0 software license, but the GPLv3 is
fairly common among Emacs users. Except where specifically mentioned
the code in this repository is licensed under the GPLv3. If you are an
author of code I have used and do not like this license, please
contact me and I will call that out more clearly.
