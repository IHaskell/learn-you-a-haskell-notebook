# Initialized with
#
#    nix flake init --template github:tweag/jupyterWith
#
# https://github.com/tweag/jupyterWith/blob/c0941fe9a1a93e29e5ddc9228f9eacb263aa28cd/docs/HOWTO.md#initialize-a-project
#
{
  description = "Learn You a Haskell for Great Good! Jupyter adaptation";

  nixConfig.extra-substituters = [
    "https://tweag-jupyter.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
  ];

  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/1158501e7c7cba26d922723cf9f70099995eb755";
  inputs.jupyterWith.url = "github:tweag/jupyterWith";

  outputs = {
    self,
    flake-compat,
    flake-utils,
    nixpkgs,
    jupyterWith,
  }:
    flake-utils.lib.eachSystem
    [
      flake-utils.lib.system.x86_64-linux
    ]
    (
      system: let
        inherit (jupyterWith.lib.${system}) mkJupyterlabFromPath;
        pkgs = import nixpkgs {inherit system;};
        jupyterlab = mkJupyterlabFromPath ./kernels {inherit pkgs;};
      in rec {
        packages = {inherit jupyterlab;};
        packages.default = jupyterlab;
        apps.default.program = "${jupyterlab}/bin/jupyter-lab";
        apps.default.type = "app";
      }
    );
}
