#!/bin/sh

# These reposurgeon scripts are common to all repositories.
# They are run in order:
# 1. common_load.lift
# 2. common_merge.sh
# 3. common_clean.lift

mkdir -p tmp_combined
cd tmp_combined

git init --quiet
git config core.filemode false

git remote add tmp_early ../docutils_tmp_early
git remote add tmp_docstring ../cvs_tmp_docstring
git remote add tmp_structuredtext ../cvs_tmp_structuredtext
git fetch --all

git switch -c docutils tmp_early/master
git switch -c docstring tmp_docstring/docstring
git switch -c structuredtext tmp_structuredtext/structuredtext
git switch docutils

git merge --allow-unrelated-histories -m "~merge histories~" docutils structuredtext docstring
git read-tree docutils structuredtext docstring
git commit -m "~merge histories~"
git reset --hard

git rm -r web_docstring web_structuredtext
git commit --amend --all -m "~merge histories~"
#git rm -rf -- *
#git checkout docutils -- .
