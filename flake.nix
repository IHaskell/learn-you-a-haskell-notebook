{
  description = "Jupyter adaptation of Learn You a Haskell for Great Good! ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
    jupyterWith.url = "github:tweag/jupyterWith";
    jupyterWith.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, jupyterWith, }:
  let
    l = nixpkgs.lib // builtins;

    supportedSystems = [ "x86_64-linux" ];

    forAllSystems = f: l.genAttrs supportedSystems
      (system: f system (import nixpkgs {
        inherit system;
        overlays = l.attrValues jupyterWith.overlays;
      }));

    iHaskell = pkgs: pkgs.kernels.iHaskellWith {
      name = "learn-you-a-haskell";
      packages = p: with p; [ hvega formatting ];
    };

    defaultPackage = forAllSystems (system: pkgs:
      (pkgs.jupyterlabWith {
        kernels = [ (iHaskell pkgs) ];
      }).overrideAttrs (old: {
        passthru = old.passthru // {
          meta.mainProgram = "jupyter-lab";
        };
      })
    );

  in
  {
    inherit defaultPackage;
    devShell = forAllSystems (system: pkgs: self.defaultPackage.${system}.env);
  };
}
