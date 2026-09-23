## Architecture

This is my global emacs literate config.  The entire configuration lives in readme.org and is
tangled out to the appropriate file.


## Project Structure
- Single `.org` file containing entire emacs configuration in `readme.org` `
- Changes tangled out on save
- `user-emacs-directory` is `~/.emacs.d/`, **not** the directory `readme.org` lives in. A path
  resolved against it will not find a file tangled out beside `readme.org`.
- `.gitignore` is `*` plus `!readme.org`, so tangled output is invisible to `fd` and `rg`. "The
  file isn't there" is usually "the tool won't show it" — check with `ls` before concluding a
  tangle failed.

## Style Guide
- Single `.org` file
- One heading per package for new packages ordered alphabetically

## Never tangle in a worktree
Tangling belongs to the main checkout only. Most `:tangle` targets are relative and would just
litter the worktree, but `~/.config/rustfmt/rustfmt.toml` is **absolute** — a tangle from anywhere
writes it, so a worktree whose `readme.org` is behind can overwrite live config.

To check a change tangles and compiles before it lands, do it on a copy:

```bash
tmp=$(mktemp -d) && cp readme.org "$tmp/" && cd "$tmp" \
  && emacs --batch -l org readme.org -f org-babel-tangle \
  && emacs -Q --batch --eval '(progn (dolist (d (directory-files "~/.emacs.d/elpa" t "\\`[^.]")) (when (file-directory-p d) (add-to-list (quote load-path) d))) (byte-compile-file "init.el"))'
```

Read only the errors. `Warning: the function ... might not be defined at runtime` is normal for
anything a later `use-package` loads and is not a failure.

## Changing the live session
Evaluating into the running Emacs is fine, but it is running the *currently tangled* `init.el`,
not the working tree. Never remove or unbind a function that init still calls — under a repeating
timer that errors every tick and visibly slows the editor. Redefine the caller first, or leave
both in place.
