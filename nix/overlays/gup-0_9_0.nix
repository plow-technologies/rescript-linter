let
  pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/e89cf1c932006531f454de7d652163a9a5c86668.tar.gz";
    }) {};

  gup-0_9_0 = pkgs.gup;
in self: super: {
  gup = gup-0_9_0;
}