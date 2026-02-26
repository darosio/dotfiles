# Git Submodules

## Add New Submodules

```bash
git submodule add -b <branch> <repository> [<submodule-path>]
git submodule update --remote
```

## Remove Submodules

```bash
# Delete the relevant section from .gitmodules
git add .gitmodules
# Delete the relevant section from .git/config
git rm --cached path_to_submodule   # no trailing slash
rm -rf .git/modules/path_to_submodule
git commit -m "Removed submodule <name>"
rm -rf path_to_submodule
```

## Submodule Detached HEAD

If a submodule ends up with a detached HEAD:

```bash
cd <submodule-path>
git checkout <branch>
git branch -u refs/remotes/origin/master master
```

Check `.gitmodule` and `.git/config`, or:

```bash
cd <submodule-path>
git checkout <branch>
cd <parent-repo-path>
git config -f .gitmodules submodule.<submodule-path>.branch <branch>
```
