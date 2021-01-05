# https://stackoverflow.com/a/56180220/437583

with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs;
    ref = "master";
    rev = "1998b95adc6b5a9aef336e45955a01575bd898f1";
}) {};

haskell.lib.buildStackProject {
    name = "my-project";
    buildInputs = [
      haskell.compiler.ghc8103
      # For zlib (it is a transitive dependency).
      zlib ]; }
