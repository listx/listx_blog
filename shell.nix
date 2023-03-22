# https://stackoverflow.com/a/56180220/437583

let
    pkgs = import (builtins.fetchGit {
        url = https://github.com/NixOS/nixpkgs;
        ref = "nixos-22.11";
        rev = "ab1254087f4cdf4af74b552d7fc95175d9bdbb49";
    }) {
#	overlays = [
#		(self: super: {
#			rustc = (super.rustc.override {
#				stdenv = self.stdenv.override {
#					targetPlatform = super.stdenv.targetPlatform // {
#						parsed = {
#							cpu = { name = "wasm32"; };
#							vendor = {name = "unknown";};
#							kernel = {name = "unknown";};
#							abi = {name = "unknown";};
#						};
#					};
#				};
#			}).overrideAttrs (attrs: {
#				configureFlags = attrs.configureFlags ++ ["--set=build.docs=false"];
#			});
#		})
#	];
    };
in

pkgs.haskell.lib.buildStackProject {
    name = "my-project";
    buildInputs = [
      # pkgs.haskell.compiler.ghc8107
      #pkgs.rustc
      # For zlib (it is a transitive dependency).
      pkgs.zlib ]; }
