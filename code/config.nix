# Taken from
# http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015601.html.
{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellngPackages = super.haskellngPackages.override {
      overrides = self: super: {

        # Enable profiling. Taken from
        # http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015620.html.
        # Comment out this line if you do not want to enable profiling!
        mkDerivation = expr: super.mkDerivation (expr // {
          enableLibraryProfiling = true; });

        # Private package
        ztile = self.callPackage ./my-local-hs/ppx.nix {};
      };
    };
  };
}
