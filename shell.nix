let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc883";
  hnix = import sources.iohk-hnix {};
  hls = import sources.all-hls { version = "0.4.0"; ghc = "8.8.3"; };
  pkgs = (import hnix.sources.nixpkgs-2003) hnix.nixpkgsArgs;
in
(import ./.).shellFor {
  withHoogle = true;
  buildInputs = [
    hls
    (pkgs.haskell-nix.tool compilerVersion "cabal-install" { version = "3.2.0.0"; })
    (pkgs.haskell-nix.tool compilerVersion "hpack"         { version = "0.34.2"; })
    (pkgs.haskell-nix.tool compilerVersion "ghcid"         { version = "0.8.7";  })
  ];
}
