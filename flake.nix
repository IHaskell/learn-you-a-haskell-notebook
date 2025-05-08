{
  description = "Learn You a Haskell for Great Good! Jupyter adaptation";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.IHaskell.url = "github:IHaskell/IHaskell";

  nixConfig = {
    extra-substituters = [ "https://ihaskell.cachix.org" ];
    extra-trusted-public-keys = [ "ihaskell.cachix.org-1:WoIvex/Ft/++sjYW3ntqPUL3jDGXIKDpX60pC8d5VLM="];
  };

  outputs = {
    self,
    flake-utils,
    IHaskell,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = IHaskell.inputs.nixpkgs24_11.legacyPackages.${system};

        ihaskell-env = IHaskell.packages.${system}.ihaskell-env-display-ghc98;
        notebook-lyah = pkgs.stdenv.mkDerivation {
          name = "notebook-lyah";
          src = ./notebook;
          phases = [ "unpackPhase" "installPhase" ];
          installPhase = ''
            mkdir -p $out
            cp -r $src/* $out/
          '';
        };
      in rec {
        packages = {
          inherit ihaskell-env notebook-lyah;
        };
        apps = {
          default =
            let
              script = pkgs.writeShellApplication {
                name = "jupyter-lab-lyah";
                runtimeInputs = [ ihaskell-env notebook-lyah ];
                text = "jupyter lab --notebook-dir=${notebook-lyah} ${notebook-lyah}/00-preface.ipynb";
              };
            in
            {
              type = "app";
              program = "${script}/bin/jupyter-lab-lyah";
            };
        };
      }
    );
}
