=====================
 README: The Sandbox
=====================

The Sandbox_ is a place to play around, to try out and share ideas.
It's a part of the CVS repository but it isn't distributed as part of
Docutils_ releases.  Each developer who wants to play here should
create their own subdirectory (suggested name: SourceForge ID, or
given name + family initial).

It's OK to make a mess here!  But please, play nice.

For more information, please see the `Docutils Project Policies`_.

.. _Sandbox: ./
.. _Docutils: ../
.. _Docutils Project Policies: ../spec/notes.html#project-policies


Sandbox Projects
================

.. contents:: :local:

Project Infrastructure
----------------------

`docutils-update <davidg/infrastructure/docutils-update>`_ is
a script that is installed as a cron job on SourceForge to
automatically update the Docutils_ web site whenever the CVS files
change.  Any .html document with a corresponding .txt file is
regenerated whenever the .txt file changes.
