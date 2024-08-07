#!/bin/sh
set -e

cd "$1"

# Reinitialise repository. We do this for sanity, as git might expect
# things that reposurgeon doesn't provide.
git init

# Run a garbage-collect on the generated git repository.  Import doesn't.
# This repack call is the active part of gc --aggressive.  This call is
# tuned for very large repositories.
git -c pack.threads=1 repack -AdF --window=1250 --depth=250

# Clean repo
git prune
git rm -rf .
git clean -fxd
git reset HEAD --hard

# Check integrity
git fsck --full --strict --unreachable --dangling --lost-found

# Enable commit graph
git config core.commitGraph true
git config fetch.writeCommitGraph true
git commit-graph write --append --reachable --changed-paths
git commit-graph verify

# End by re-reinitialising repository. Why not!
git init
