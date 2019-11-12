# https://stackoverflow.com/a/56180220/437583

with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-19.03";
}) {};

haskell.lib.buildStackProject {
    name = "my-project";
    buildInputs = [
      haskell.compiler.ghc844
      # For zlib (it is a transitive dependency).
      zlib ]; }
