# Tools

- Prefer `rg` over `grep` and `git grep`.
  `rg` excludes `.git/` and respects gitignore files by default.

- Prefer `fd` over `find`.
  `fd` excludes `.git/` and respects gitignore files by default.

# Git

- A branch's upstream (what it is based on and pulls from) and its push target are separate.
  For a branch `feature` based on `origin/main`:

  ```
  [branch "feature"]
  	remote = origin
  	merge = refs/heads/main
  ```

  `remote.pushDefault = origin` and `push.default = current` are set globally, so `feature` pushes to `origin/feature`.

- To find a branch's upstream, use `@{upstream}` (`git rev-parse --abbrev-ref feature@{upstream}`).
  Don't infer it from the branch name.

- To find where a branch is pushed, use `@{push}` (`git rev-parse --abbrev-ref feature@{push}`).
  To check whether it has been pushed, see if that ref exists and contains the local tip (`git merge-base --is-ancestor feature feature@{push}`).
  An upstream of `origin/main` does not mean the branch has been pushed.

- Don't use `git push -u`/`--set-upstream` or `--track` since they overwrite the upstream with the pushed branch.
  Use a plain `git push`.
