#!/bin/sh

# exit on error or undefined variable; echo commands
set -eux

# These reposurgeon scripts are common to all repositories.
# They are run in order:
# 1. common_load.lift
# 2. common_cvs_docstring.lift
# 3. common_cvs_structuredtext.lift
# 4. common_combine.sh
# 5. common_clean.lift

mkdir tmp_combined
cd tmp_combined

git init --quiet
git config core.filemode false
git config user.email "goodger@python.org"
git config user.name "David Goodger"

git remote add tmp_early ../docutils_tmp_early
git remote add tmp_docstring ../cvs_tmp_docstring
git remote add tmp_structuredtext ../cvs_tmp_structuredtext
git fetch --all

git switch -c docutils tmp_early/master
git switch -c docstring tmp_docstring/master-docstring
git switch -c structuredtext tmp_structuredtext/master-structuredtext

# This will claim to fail, hence using read-tree
# https://stackoverflow.com/a/31186732
git switch docutils
git merge --allow-unrelated-histories -m "~merge histories~" docutils structuredtext docstring || true
git read-tree docutils structuredtext docstring
# Remove the old web directories
git rm -r web_docstring web_structuredtext
# Commit the index and reset the working tree
git commit -m "~merge histories~"
git reset --hard

git branch --delete docstring structuredtext
git remote remove tmp_early
git remote remove tmp_docstring
git remote remove tmp_structuredtext
