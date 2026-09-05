{
  nixConfig = {
    bash-prompt = "\[intelli-monad$(__git_ps1 \" (%s)\")\]$ ";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, nixpkgs, utils, flake-compat  }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
        # The three repo packages, built from their cabal files.
        # ghc98 keeps the flake on the same compiler grade the CI
        # matrix tests (9.8.x).
        haskellPkgsBase = pkgs.haskell.packages.ghc98;
        # Pin the packages whose nixpkgs versions fall outside the
        # bounds the suite actually tests against (nixpkgs ships
        # louter-0.1.1.1, megaparsec-9.7+, persistent-2.17). callHackage
        # pulls our tested versions; first full build compiles them
        # from source.
        haskellPkgs = haskellPkgsBase.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
            (self: super: {
              # louter-0.1.1.2 postdates this pin's all-cabal-hashes, so
              # callHackage can't see it — fetch the Hackage tarball
              # directly instead.
              louter = self.callCabal2nix "louter" (pkgs.fetchzip {
                url = "https://hackage.haskell.org/package/louter-0.1.1.2/louter-0.1.1.2.tar.gz";
                sha256 = "sha256-MT8N4B7ETHD6iG0ILwd9UUT9wIIO4hBzUnEeURyaOx8=";
              }) {};
              megaparsec = self.callHackage "megaparsec" "9.6.1" {};
              # Consistent persistent pair inside our cabal bounds
              # (< 2.15): persistent 2.14.6.3 + persistent-sqlite
              # 2.13.3.0 (nixpkgs' 2.13.3.1 wants persistent >= 2.15.2,
              # and nixpkgs' plain persistent is 2.17). persistent-test
              # (a dep of persistent-sqlite's test suite) conflicts with
              # this pairing, so the suite is skipped — our suite is
              # the gate that matters.
              persistent = pkgs.haskell.lib.dontCheck
                (self.callHackage "persistent" "2.14.6.3" {});
              # persistent-sqlite must NOT use any nix-generated expression
              # as-is (hackage2nix, callHackage, or a fresh local
              # cabal2nix — all three verified identical): cabal2nix
              # force-enables the `systemlib` flag for this package and
              # then maps the external `sqlite3` link directive to the
              # *Haskell* package `sqlite` (ancient 0.5.5, base < 4.11,
              # unbuildable) in librarySystemDepends. Fix: drop the forced
              # flag (default flags vendor the C library, exactly like
              # our cabal/stack builds) and clear the bogus system dep.
              persistent-sqlite = pkgs.haskell.lib.dontCheck
                (pkgs.haskell.lib.overrideCabal
                  (self.callCabal2nix "persistent-sqlite"
                    (pkgs.fetchzip {
                      url = "https://hackage.haskell.org/package/persistent-sqlite-2.13.3.0/persistent-sqlite-2.13.3.0.tar.gz";
                      sha256 = "sha256-waw3RVisHum7uaHo/z1UtSvHWjh+Kv9OrDY6KoHEozc=";
                    }) {})
                  (old: { librarySystemDepends = []; configureFlags = []; }));
            });
        });
        intelli-monad = haskellPkgs.callCabal2nix "intelli-monad" ./intelli-monad {};
        openai-servant-gen = haskellPkgs.callCabal2nix "openai-servant-gen" ./openai-servant-gen {};
      in
      {
        packages = {
          inherit intelli-monad;
          default = intelli-monad;
        };

        apps = {
          default = {
            type = "app";
            program = "${intelli-monad}/bin/repl";
          };
          repl = {
            type = "app";
            program = "${intelli-monad}/bin/repl";
          };
          mcp-serve = {
            type = "app";
            program = "${intelli-monad}/bin/mcp-serve";
          };
        };

        devShell = with pkgs; mkShell {
          buildInputs = [
            git
            cabal-install
            zlib
            zstd
            haskell.compiler.ghc983
          ];
          shellHook = ''
            source ${git}/share/bash-completion/completions/git-prompt.sh
          '';
        };
      });
}
