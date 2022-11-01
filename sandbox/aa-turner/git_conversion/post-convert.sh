#!/bin/sh
set -e

cd "$1"

# Ignore filemode changes
git config core.filemode false

# Run a garbage-collect on the generated git repository.  Import doesn't.
# This repack call is the active part of gc --aggressive.  This call is
# tuned for very large repositories.
git -c pack.threads=1 repack -AdF --window=1250 --depth=250

# Clean repo
git prune
git clean -fxd
git reset HEAD --hard

# Check integrity
git fsck --full --dangling --unreachable --strict

# Enable commit graph
git config core.commitGraph true
git show-ref -s | git commit-graph write --stdin-commits
