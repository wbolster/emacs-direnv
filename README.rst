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
