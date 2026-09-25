## Architecture

My global Emacs config. Plain elisp, split by domain. A flake pins the package set and owns
nothing else.

This was a literate org config that tangled into `init.el`. It is not any more. `readme.org` is
orientation only — a map of the tree, not the build source. Nothing here tangles, and nothing
here writes outside the repo.

## Project Structure

- `init.el` at the root loads every file under `config/` and `lisp/` in a **fixed order**, listed
  explicitly. That order is the order the old literate config tangled into, so behaviour does not
  depend on the grouping.
- `config/` is package setup, `config/org/` the org sections, `lisp/` my own commands.
- Adding a package means editing the domain file it belongs in and adding a `(ads/load-config ...)`
  line only if you created a new file. Put the new file at the position its dependencies allow.
- Files are loaded by explicit path, never `require`. `config/org/org.el` and `config/dired.el`
  would shadow the real packages the moment their directory joined `load-path`.

## Config paths vs state paths

`user-emacs-directory` is **not** where the config lives. After cutover `~/.emacs.d/init.el` is a
symlink into this repo, so `user-emacs-directory` is `~/.emacs.d` and the elisp is somewhere else.

- Config resolves against `ads/config-directory`, which is `file-truename` of `load-file-name`.
- State (`custom.el`, `eln-cache`, history, bookmarks) keeps using `user-emacs-directory`.

Getting this backwards works fine under `--init-directory` and breaks at cutover, which is the
worst possible time to find out.

## Nix

`flake.nix` parses every `use-package` form under `config/` and `lisp/` with `alwaysEnsure`, so
each one must name a real package.

- `nix build .#packageNames` lists what resolved, without building Emacs. Run it after touching a
  `use-package` form. **Check the source, not just that it resolved** — the overlay warns rather
  than fails on a name it cannot place, and will hand over a different package that happens to
  share the name. It was already doing that to `org-download`.
- A form for something built into Emacs needs `:ensure nil`, or the parser reports it missing.
- A package installed with `:vc` is not on MELPA and needs an entry in `gitSources` with a rev, a
  hash and a version. `nix flake prefetch --json github:owner/repo` gives the first two. The
  version is MELPA's `YYYYMMDD.HHMM` of that commit with **no leading zero** in the time —
  package.el reads `0627` as `627` and then refuses the tarball.
- `package.el` runs, deliberately. The flake ships packages as an elpa tree in the store and
  `package-activate-all` is the only thing that loads their autoloads; turning it off leaves every
  package on `load-path` with every autoloaded command undefined. What must not change is
  `package-user-dir`, which is pointed away from `~/.emacs.d/elpa` so the old unmanaged tree
  cannot get in front of the pinned set.

## Changing the live session

Evaluating into the running Emacs is fine, but until cutover that Emacs is running the **tangled
`init.el` from the `~/git/emacs` main checkout**, not this tree. Never remove or unbind a function
that init still calls — under a repeating timer that errors every tick and visibly slows the
editor. Redefine the caller first, or leave both in place.

## Trying a change out goes in a temp file

Don't edit a tracked file to test an idea. Write the change to a dedicated temporary `.el` with a
descriptive name, and end the message with the load line and nothing after it:

```elisp
(load-file "/tmp/test-changes.el")
```

## Must not touch

- `~/git/emacs`, the main checkout. It holds the live tangled `init.el` and is the running editor.
- The `~/.emacs.d` symlinks, until cutover. That is his call, never part of the work.

## Style

- One file per domain; a section over ~150 lines earns its own.
- Prose in `readme.org` stays to about a sentence.
