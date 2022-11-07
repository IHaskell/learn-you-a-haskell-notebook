{
  name,
  availableKernels,
  extraArgs,
}:
availableKernels.haskell {
  inherit name;
  inherit (extraArgs) pkgs;
  displayName = "Haskell";
}
