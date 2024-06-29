let
  # Nixpkgs snapshot.
  sources = import ./nix/sources.nix;
  # Overlays
  oxalica-rust-overlay = import sources.rust-overlay;
  # Build rust crates.
  naersk = final: prev: {
    naersk = pkgs.callPackage sources.naersk
      {
        inherit sources;
      };
    };
  # The final "pkgs" attribute with all the bells and whistles of our overlays.
  pkgs = import sources.nixpkgs {
    overlays = [
      oxalica-rust-overlay
      naersk
    ];
  };

  darwin-cargo-build-rustflags = pkgs.lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
    CARGO_BUILD_RUSTFLAGS = "-C link-arg=-undefined -C link-arg=dynamic_lookup";
  };
in

# This is our development shell.
pkgs.mkShell ({
  buildInputs = [
    # Haskell
    pkgs.haskell.compiler.ghc982
    pkgs.cabal-install
    # Haskell libs need zlib.
    pkgs.zlib

    # Rust
    # For now just run `gen-js` from inside the `nix-shell` in order to install
    # Rust via rustup and to eventually generate the WASM code from Rust
    # ("rust-js/js/rust_js_bg.wasm" and "rust-js/js/rust_js.js").

    #pkgs.rust-bin.stable.latest.default
    pkgs.rustup
    pkgs.wasm-pack

    # For updating Nix dependencies.
    pkgs.niv

    # Misc
    pkgs.git
    pkgs.less
  ]
  # For file_system on macOS.
  ++ pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreFoundation
    CoreServices
    Security
  ]);
} // darwin-cargo-build-rustflags)
