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

          buildFromGit = epkgs: pname: spec: epkgs.melpaBuild {
            inherit pname;
            inherit (spec) version;
            commit = spec.rev;
            src = pkgs.fetchFromGitHub {
              inherit (spec) owner repo rev hash;
            };
            recipe = pkgs.writeText "recipe" ''
              (${pname} :fetcher github :repo "${spec.owner}/${spec.repo}")
            '';
            packageRequires = spec.deps epkgs;
          };

          emacsPackage = pkgs.emacs;
        in
        rec {
          default = emacs;

          emacs = pkgs.emacsWithPackagesFromUsePackage {
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
          };

          # The package set the parser resolved, so it can be read without
          # building Emacs.  Anything the overlay could not place comes back
          # as null and is traced during evaluation.
          packageNames = pkgs.writeText "emacs-package-names"
            (lib.concatStringsSep "\n"
              (lib.unique (lib.sort (a: b: a < b)
                (map (p: p.pname or p.name or "?")
                  (lib.filter (p: p != null) emacs.explicitRequires)))));
        });

      devShells = forEachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [ self.packages.${pkgs.system}.emacs ];
        };
      });

      formatter = forEachSystem (pkgs: pkgs.nixpkgs-fmt);
    };
}
