{pkgs, ...}: {
  # kernel.python.minimal = {
  #   enable = true;
  # };
  kernel.haskell."1" = {
    enable = true;
    displayName = "Haskell";
  };
}
