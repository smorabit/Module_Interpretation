#!/usr/bin/env bash
# rebuilds the pkgdown site locally (needs the local hdWGCNA test object,
# which is why this cannot run in GitHub Actions) and pushes the output
# to the gh-pages branch via the worktree at ../llegir-gh-pages
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
worktree_dir="$repo_root/../llegir-gh-pages"

if [ ! -d "$worktree_dir" ]; then
    echo "worktree not found at $worktree_dir -- run:"
    echo "  git worktree add ../llegir-gh-pages gh-pages"
    exit 1
fi

cd "$repo_root"
Rscript -e 'pkgdown::build_site(preview = FALSE)'

rsync -a --delete --exclude .git "$repo_root/pkgdown_site/" "$worktree_dir/"

cd "$worktree_dir"
git add -A
if git diff --cached --quiet; then
    echo "no changes to deploy"
    exit 0
fi
git commit -m "deploy: rebuild pkgdown site $(date -u +%Y-%m-%dT%H:%M:%SZ)"
git push origin gh-pages
