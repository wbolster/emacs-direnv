============
emacs-direnv
============

.. image:: https://melpa.org/packages/direnv-badge.svg
   :alt: melpa badge

.. image:: https://stable.melpa.org/packages/direnv-badge.svg
   :alt: melpa stable badge

.. _direnv: https://direnv.net/

this package provides direnv_ integration for emacs.

it works by invoking
``direnv`` to obtain the environment
for the current file,
then updating the emacs variables
``process-environment`` and ``exec-path``.

the result is that
programs started from within emacs,
such as inferior shells, linters, compilers, and test runners,
will be looked up in the correct ``$PATH``,
and will be started
with the correct environment variables set.

installation
============

::

  M-x package-install RET direnv RET

alternatively, put the elisp file
somewhere in the loading path
and load it explicitly:

.. code-block:: elisp

  (require 'direnv)

also make sure
that the direnv version (``direnv version``)
is at least 2.8.0
since this package uses
the json export capabilities (``direnv export json``).

usage
=====

the command ``direnv-update-environment``
provides the core functionality of this package:
it updates the emacs environment
to the direnv environment for the current file.
the minibuffer will show a message
with a summary of the changes made to the environment,
similar to what ``direnv`` does in a shell.

the global minor mode ``direnv-mode`` does the same,
but automatically updates the emacs environment
when the active buffer changes,
so that the environment always matches the current file.
to automatically enable this behaviour when emacs starts,
put this in your ``~/.emacs/init.el``:

.. code-block:: elisp

  (direnv-mode)

additionally, the ``direnv-edit`` command
acts like ``direnv edit`` from a shell:
it edits the ``.envrc`` file
associated with the current directory
or one of its parent directories.

configuration
=============

to avoid visual distraction,
no summary messages are displayed in the minibuffer
when ``direnv-mode`` automatically changes the environment,
since the environment may change on every buffer switch.
if you prefer to see the summary message anyway,
change the ``direnv-always-show-summary`` variable:

.. code-block:: elisp

  (setq direnv-always-show-summary t)

the summary message contains
the paths of the old and new directories,
which may be a bit too verbose for your taste.
the ``direnv-show-paths-in-summary`` variable
controls whether the summary includes path names.
to get shorter summary messages, use:

.. code-block:: elisp

  (setq direnv-show-paths-in-summary nil)

the summary message uses different font faces
for added, changed, and removed environment variables.
depending on your theme,
this usually results in different colours.
this behaviour can be disabled
by changing the ``direnv-use-faces-in-summary`` variable:

.. code-block:: elisp

  (setq direnv-use-faces-in-summary nil)

these settings can also be changed
using the customize interface::

  M-x customize-group RET direnv RET


contributing
============

praise? complaints? bugs? questions? ideas?

please use the github issue tracker.


credits
=======

this emacs package was created by
`wouter bolsterlee (@wbolster)
<https://github.com/wbolster>`_.

it incorporates ideas from earlier
packages created by
`jonathan lange (@jml)
<https://github.com/jml>`_
and
`christian romney (@christianromney)
<https://github.com/christianromney>`_.


license
=======

(this is the 2-clause bsd license.)

copyright 2017 wouter bolsterlee

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

this software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. in no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
