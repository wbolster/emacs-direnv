============
emacs-direnv
============

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
updates the current environment
so that it matches the current file.

the global minor mode ``direnv-mode`` does the same,
but automatically updates the emacs environment
when the active buffer changes.

the easiest way to use this package
is to enable ``direnv-mode``
when emacs starts
by putting this in ``~/.emacs/init.el``:

.. code-block:: elisp

  (direnv-mode)

in addition to the global minor mode,
this package provides the following commands,
which can be invoked using ``M-x``
or bound to a key:

* ``direnv-mode``
  enables or disables the global minor mode.

* ``direnv-update-environment``
  updates the emacs environment
  to the direnv environment for the current file.

* ``direnv-edit``
  acts like ``direnv edit`` from a shell;
  it edits the ``.envrc`` file
  associated with the current directory
  or one of its parent directories.

installation
============

::

  M-x package-install RET direnv RET

fixme: there is already another direnv package on melpa
so this is not going to work without upfront coordination :(

alternatively, put the elisp file
somewhere in the loading path
and load it explicitly:

.. code-block:: elisp

  (require 'direnv)
