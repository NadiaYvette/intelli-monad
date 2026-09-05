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
        haskellPkgs = pkgs.haskell.packages.ghc98;
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
