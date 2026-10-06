{
  description = "Emacs with the package set this configuration asks for";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forEachSystem = f: nixpkgs.lib.genAttrs systems (system: f (
        import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        }
      ));

      # Packages the config installs with use-package :vc, which the
      # overlay cannot resolve because they are not on MELPA or ELPA.
      # Bumping one means a new rev, hash and version here;
      # `nix flake prefetch --json github:owner/repo` gives the first two.
      #
      # version is MELPA's YYYYMMDD.HHMM of the pinned commit, and the time
      # must carry no leading zero.  package.el reads 0627 as 627 and then
      # refuses the tarball for unpacking under a name it did not expect.
      gitSources = {
        agent-shell-knockknock = {
          version = "20260316.1504";
          owner = "xenodium";
          repo = "agent-shell-knockknock";
          rev = "56732434067fe1874dcda62c491f7800bdc0a2f3";
          hash = "sha256-R7bvk2v9togbPiGKOXDAKCMStrL1bbRJoTdBtj0PdlU=";
          deps = p: [ p.agent-shell p.knockknock ];
        };
        "bookmark+" = {
          version = "20260902.2033";
          owner = "emacsmirror";
          repo = "bookmark-plus";
          rev = "39fb6818fc102c17cf15147d5c36810c59cd9ebb";
          hash = "sha256-W1z8J2FDCvq6uPGvfpbw2mKxK0Y9/qSfhzD8zybVhSg=";
          deps = _: [ ];
        };
        kanata-kbd-mode = {
          version = "20250902.1900";
          owner = "chmouel";
          repo = "kanata-kbd-mode";
          rev = "0315b567bd61951433c3bdb8e59160d77e1fdcda";
          hash = "sha256-nh62MfmlUGZAW8Mk8ooNoStbSIzAdOhH6PTzqAD8aHs=";
          deps = p: [ p.consult ];
        };
        knockknock = {
          version = "20260316.1540";
          owner = "xenodium";
          repo = "knockknock";
          rev = "7a6ab46503554317b639a7333ec8046d7d181520";
          hash = "sha256-hkvEuad3Gh++PMeaMJHd2j//ho+FA59QGxCex3NVi98=";
          deps = p: [ p.posframe p.nerd-icons ];
        };
        org-modern-indent = {
          version = "20260721.2333";
          owner = "jdtsmith";
          repo = "org-modern-indent";
          rev = "86bd83ee1ad95f123810eb3b116beb543db1960a";
          hash = "sha256-vQzYk5qejCBehpbxkMceOMsmeLyjnAstpezZw/ZR1jQ=";
          deps = p: [ p.compat ];
        };
        org-timegrid = {
          version = "20260921.627";
          owner = "Gleek";
          repo = "org-timegrid";
          rev = "258e49c9c6f105ac3600f4e7f937ed90d87b90a3";
          hash = "sha256-ouGBcsdL00f5lzDg88TzfiFo8wY2ws+q6F0AYwLx5Hk=";
          deps = _: [ ];
        };
        toggl = {
          version = "20260925.2144";
          owner = "andanao";
          repo = "emacs-toggl-track";
          rev = "dd01ae4f890ddb50b2da4b2999b919080b627113";
          hash = "sha256-Lyy8C2u2920vCz2qTjps9orlA3mvC4DHSPpdMfZBP8o=";
          deps = p: [ p.plz p.transient ];
        };

        # The config pins a personal fork, and the overlay would otherwise
        # hand over abo-abo's upstream without saying so.
        org-download = {
          version = "20250430.1854";
          owner = "andanao";
          repo = "org-download";
          rev = "7387a584b6308e6713350b76e3f27cdbb8ca2097";
          hash = "sha256-CtiU0tYL3bxgrmYqZLXgtI6PWC6e93wD3bC0n3Gp8fs=";
          deps = p: [ p.async ];
        };

        # dakra/ghostel is a Zig terminal that carries its Emacs client in
        # lisp/.  The overlay's package builds the terminal too, which needs
        # Ghostty's vendored Zig dependencies and fails fetching them.
        # package-vc only ever took the elisp, so that is what is taken here.
        # The native module stays a system concern, as it already was.
        ghostel = {
          version = "20260921.1634";
          owner = "dakra";
          repo = "ghostel";
          rev = "c2c411f2b0051465a5f5e7826ebab4ed216d4c0e";
          hash = "sha256-IHcFxvkCKz/YANJrrHWFOiACfkHQJsN6YOnX8cLqi1A=";
          files = [ "lisp/*.el" ];
          deps = p: [ p.compat ];
          # etc/terminfo carries the xterm-ghostty entry ghostel sets TERM to.
          # Without it every terminal falls back to xterm-256color and redraws
          # badly.  `ghostel--resource-root' looks for etc/ beside the elisp,
          # which for a flat MELPA install is the package directory itself.
          postInstall = ''
            cp -r "$src/etc" "$out"/share/emacs/site-lisp/elpa/ghostel-*/
          '';
        };
        evil-ghostel = {
          version = "20260921.1634";
          owner = "dakra";
          repo = "ghostel";
          rev = "c2c411f2b0051465a5f5e7826ebab4ed216d4c0e";
          hash = "sha256-IHcFxvkCKz/YANJrrHWFOiACfkHQJsN6YOnX8cLqi1A=";
          files = [ "extensions/evil-ghostel/*.el" ];
          deps = p: [ p.evil p.ghostel ];
        };
      };
    in
    {
      packages = forEachSystem (pkgs:
        let
          inherit (pkgs) lib;

          # emacsWithPackagesFromUsePackage parses one config input, so the
          # many files have to arrive as one string.  Only config/ and lisp/
          # are read: the per-system files at the repo root declare packages
          # for machines this set is not built for.
          elispFiles = lib.sort (a: b: toString a < toString b)
            (lib.filter (p: lib.hasSuffix ".el" (toString p))
              (lib.filesystem.listFilesRecursive ./config
                ++ lib.filesystem.listFilesRecursive ./lisp));

          parsedConfig = lib.concatMapStringsSep "\n" builtins.readFile
            elispFiles;

          # `postInstall' is for sources that ship more than elisp.  The MELPA
          # :files spec flattens what it matches, which would lose a directory
          # tree, so anything shaped has to be copied across by hand.
          buildFromGit = epkgs: pname: spec: epkgs.melpaBuild ({
            inherit pname;
            inherit (spec) version;
            commit = spec.rev;
            src = pkgs.fetchFromGitHub {
              inherit (spec) owner repo rev hash;
            };
            recipe = pkgs.writeText "recipe" ''
              (${pname} :fetcher github :repo "${spec.owner}/${spec.repo}"${
                lib.optionalString (spec ? files)
                  (" :files (" + lib.concatMapStringsSep " "
                    (f: ''"${f}"'') spec.files + ")")
              })
            '';
            packageRequires = spec.deps epkgs;
          } // lib.optionalAttrs (spec ? postInstall) {
            inherit (spec) postInstall;
          });

          emacsPackage = pkgs.emacs;

          # Programs the config shells out to, pinned by this flake like the
          # elisp is.  Only subprocesses belong here: jinx links against
          # enchant at build time rather than calling it, so listing enchant
          # would do nothing, and jinx and pdf-tools already ship their built
          # binaries.
          runtimeTools = with pkgs; [
            d2                  # config/d2.el renders diagrams
            imagemagick         # `magick', image conversion
            zig                 # builds ghostel's native module
            rust-analyzer       # lsp, Rust
            basedpyright        # lsp, Python
          ];
        in
        rec {
          default = emacs;

          # Put `runtimeTools' on Emacs' own PATH.  Newer nixpkgs do this for
          # the package tree's bin/, but the pinned one only sets
          # EMACSLOADPATH, so the prepend has to happen here.
          #
          # This matters most for the macOS app bundle: launched from Finder
          # or `open' it inherits no shell PATH at all, which is why mac.el
          # patches `exec-path' by hand for TeX.  Wrapping the bundle's own
          # binary is the only thing that reaches that case.
          emacs = pkgs.runCommand "${emacsUnwrapped.name}-with-tools"
            {
              nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
              inherit (emacsUnwrapped) meta;
            }
            ''
              cp -a ${emacsUnwrapped} $out
              chmod -R u+w $out
              for p in "$out"/bin/emacs "$out"/bin/emacs-* \
                       "$out"/bin/emacsclient \
                       "$out/Applications/Emacs.app/Contents/MacOS/Emacs"; do
                [ -e "$p" ] || continue
                wrapProgram "$p" --prefix PATH : "${lib.makeBinPath runtimeTools}"
              done
            '';

          emacsUnwrapped = pkgs.emacsWithPackagesFromUsePackage {
            config = parsedConfig;
            package = emacsPackage;

            # Mirrors the use-package-always-ensure the config used to set
            # at runtime: every use-package form names a package to supply.
            alwaysEnsure = true;

            # Nix supplies packages only.  The configuration stays in this
            # repo and is loaded with --init-directory, so nothing of ours
            # goes into the store as an init file.
            defaultInitFile = false;

            # Self-referential, because agent-shell-knockknock depends on
            # knockknock and both are built here.
            override = epkgs:
              let built = lib.mapAttrs (buildFromGit (epkgs // built))
                gitSources;
              in epkgs // built;

            # Every grammar nixpkgs ships, so no mode ever stops to ask
            # "Tree-sitter grammar for X is missing; install it?" and
            # compile one from git.  The list in prog.el named only rust
            # and python, which is how markdown got the prompt.
            extraEmacsPackages = epkgs: [
              epkgs.treesit-grammars.with-all-grammars
            ];
          };

          # The faces config/theme.el names, pinned so a new machine gets
          # the same ones.  All libre: the Mac has nicer humanist sans faces
          # (Optima, Seravek) but they are Apple's and cannot be shipped.
          #
          # Emacs finds these through the OS, and macOS has no fontconfig,
          # so being in the store is not enough - `nix run .#install-fonts'
          # links them where the OS looks.
          fonts = pkgs.symlinkJoin {
            name = "emacs-fonts";
            paths = with pkgs; [
              nerd-fonts.lilex            # "Lilex Nerd Font", the mono
              nerd-fonts.fira-code        # kept as the fallback behind it
              nerd-fonts.symbols-only     # glyphs for any font lacking them
              emacs-all-the-icons-fonts   # all-the-icons is used too
              et-book                     # ETBembo, the serif
              source-sans                 # Source Sans 3
              atkinson-hyperlegible-next
              inter                       # registers as "Inter Variable"
              libertinus                  # Libertinus Sans, nearest Optima

              # CJK, one per role.  Simplified Chinese cuts: han glyphs differ
              # by language and the JP forms were wrong for reading Chinese.
              maple-mono.NF-CN            # "Maple Mono NF CN" - mono: code,
                                          # terminals, src blocks, tables
              lxgw-wenkai                 # "LXGW WenKai" - serif: a Kai face,
                                          # brush script, for reading prose
              noto-fonts-cjk-sans         # "Noto Sans CJK SC" - sans
            ];
          };

          # The package set the parser resolved, so it can be read without
          # building Emacs.  Anything the overlay could not place comes back
          # as null and is traced during evaluation.
          packageNames = pkgs.writeText "emacs-package-names"
            (lib.concatStringsSep "\n"
              (lib.unique (lib.sort (a: b: a < b)
                (map (p: p.pname or p.name or "?")
                  (lib.filter (p: p != null)
                    emacsUnwrapped.explicitRequires)))));
        });

      # Put the pinned fonts where the OS looks for them.
      #
      # Copies rather than symlinks: macOS CoreText ignores symlinked font
      # files outright, so a linked font silently never appears.  It does
      # read subdirectories, so everything goes in one directory this owns
      # and wipes each run - stale faces cannot pile up, and nothing
      # hand-installed alongside is touched.
      apps = forEachSystem (pkgs: {
        install-fonts = {
          type = "app";
          program = toString (pkgs.writeShellScript "install-fonts" ''
            set -eu
            src=${self.packages.${pkgs.system}.fonts}
            case "$(uname)" in
              Darwin) root="$HOME/Library/Fonts" ;;
              *)      root="''${XDG_DATA_HOME:-$HOME/.local/share}/fonts" ;;
            esac
            dest="$root/nix-emacs"
            rm -rf "$dest"
            mkdir -p "$dest"
            # .ttc/.otc are collections - several faces in one file, which
            # is how the CJK families ship.  Matching only .otf/.ttf drops
            # them silently and the families never appear.
            #
            # `cp -t' is GNU-only; BSD cp on macOS wants the destination last.
            find -L "$src" \( -name '*.otf' -o -name '*.ttf' \
                           -o -name '*.otc' -o -name '*.ttc' \) \
              -exec cp -L {} "$dest" ';'
            # Store files are read-only; the next run has to be able to
            # delete these.
            chmod -R u+w "$dest"
            echo "installed $(ls "$dest" | wc -l | tr -d ' ') font files into $dest"
            [ "$(uname)" = Darwin ] || fc-cache -f "$dest" >/dev/null 2>&1 || true
          '');
        };
      });

      devShells = forEachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [ self.packages.${pkgs.system}.emacs ];
        };
      });

      formatter = forEachSystem (pkgs: pkgs.nixpkgs-fmt);
    };
}
