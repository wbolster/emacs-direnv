============
emacs-direnv
============

.. image:: https://melpa.org/packages/direnv-badge.svg
   :alt: melpa badge

.. image:: https://stable.melpa.org/packages/direnv-badge.svg
   :alt: melpa stable badge

this package provides direnv_ integration for emacs.

.. _direnv: https://direnv.net/

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

to get started, add this to your ``init.el``:

.. code-block:: elisp

  (use-package direnv
   :config
   (direnv-mode))

.. image:: https://cloud.githubusercontent.com/assets/748944/23811101/c82c40d0-05d4-11e7-8a79-74e1d80fa5cf.png
   :alt: mandatory screenshot

installation
============

``direnv.el`` is `available from melpa <https://melpa.org/#/direnv>`_.

with ``use-package``:

.. code-block:: elisp

  (use-package direnv)

manually::

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
its sibling command ``direnv-update-directory-environment``
does the same for buffers that don't have an associated file.

the global minor mode ``direnv-mode`` does the same,
but automatically updates the emacs environment
when the active buffer changes,
so that the environment always matches the current file.
to automatically enable this behaviour when emacs starts,
put this in your ``~/.emacs/init.el``:

.. code-block:: elisp

  (direnv-mode)

or use the ``use-package`` ``:config`` block:

.. code-block:: elisp

  (use-package direnv
   :config
   (direnv-mode))

finally, the ``direnv-allow`` command
acts like ``direnv allow`` from a shell:
it allows loading the ``.envrc`` file
associated with the current directory
or one of its parent directories.
this command is useful for new projects
(always check whether the ``.envrc`` file is trustworthy),
or after editing the ``.envrc`` file within emacs itself.

configuration
=============

this packages offers various configuration settings.
these settings can be set in a ``use-package`` ``:custom`` block,
using ``(setq)``, or via the customize interface::

  M-x customize-group RET direnv RET

the available settings are outlined below.

* ``direnv-always-show-summary``

  when ``direnv-mode`` automatically changes the environment,
  a summary message will be shown in the minibuffer.
  summary messages of automatic changes can be suppressed
  by setting ``direnv-always-show-summary`` to ``nil``.
  interactive calls, e.g. ``direnv-update-environment``,
  will still show a summary message.

* ``direnv-show-paths-in-summary``

  the summary message contains
  the paths of the old and new directories,
  which may be a bit too verbose for your taste.
  the ``direnv-show-paths-in-summary`` variable
  controls whether the summary includes path names.
  to get shorter summary messages, use ``nil``.

* ``direnv-use-faces-in-summary``

  the summary message uses different font faces
  for added, changed, and removed environment variables.
  depending on your theme,
  this usually results in different colours.
  this behaviour can be disabled
  by setting ``direnv-use-faces-in-summary`` to ``nil``.

* ``direnv-non-file-modes``

  this is a list of modes
  where direnv will update
  even if the buffer has no file.
  examples include shells and
  interactive compilation (``comint``) buffers.
  example usage (with ``use-package``):

  .. code-block:: elisp

    (use-package foobar
     :config
     (add-to-list 'direnv-non-file-modes 'foobar-mode))


troubleshooting
===============

if you experience problems,
first check the buffer named ``*direnv*``.
this buffer contains
the output of the last ``direnv`` invocation,
which will likely contain more information
about the source of the problem.

when an error happens, the direnv stderr will
be automatically shown in the message area,
but for non-fatal problems
such as incorrect ``.envrc`` files
you may have to open this buffer manually for inspection
of the full output of the last ``direnv`` call.


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


history
=======

* 2.0.0 (2019-05-11)

  * add `direnv-allow` command; see
    `#43 <https://github.com/wbolster/emacs-direnv/pull/43>`_
  * use friendlier path formatting in summary message; see
    `#44 <https://github.com/wbolster/emacs-direnv/pull/44>`_
  * improve handling of direnv output and improve error reporting; see
    `#41 <https://github.com/wbolster/emacs-direnv/issues/41>`_ and
    `#42 <https://github.com/wbolster/emacs-direnv/pull/42>`_
  * remove broken `direnv-edit` command
    `#20 <https://github.com/wbolster/emacs-direnv/issues/20>`_

* 1.5.0 (2019-03-19)

  * handle indirect buffers correctly; see
    `#25 <https://github.com/wbolster/emacs-direnv/issues/25>`_
  * display ``direnv`` errors in the message area; see
    `#34 <https://github.com/wbolster/emacs-direnv/pull/34>`_
  * make the ``*direnv*`` buffer easier to find by removing the
    leading space
  * add ``eshell`` and ``dired`` to list of non-file-modes; see
    `#36 <https://github.com/wbolster/emacs-direnv/pull/36>`_ and
    `#33 <https://github.com/wbolster/emacs-direnv/issues/33>`_

* 1.4.0 (2018-03-01)

  * smarter default behaviour of summary messages on environment
    change; see
    `#23 <https://github.com/wbolster/emacs-direnv/issues/23>`_

* 1.3.0 (2018-02-13)

  * improved operation with non-file modes,
    such as shells and compilation buffers.
    see ``direnv-non-file-modes``.

* 1.2.1 (2017-06-22)

  * ``direnv-mode`` no longer fails when opening a file in
    a directory that does not (yet) exist.

* 1.2.0 (2017-05-01)

  * summary message now uses custom font faces (colours!)
  * improved error handling

* 1.1.0 (2017-03-09)

  * implemented summary message after updating environment,
    and configuration variables to tweak how it works.
  * expanded docs

* 1.0.1 (2017-03-08)

  * initial release with basic functionality


license
=======

*(this is the osi approved 3-clause "new bsd license".)*

copyright 2017 wouter bolsterlee

all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* neither the name of the author nor the names of the contributors may be used
  to endorse or promote products derived from this software without specific
  prior written permission.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
