let
  pkgs = import ./packages.nix {};
in
  { haskell-curriculum = pkgs.haskellPackages.haskell-curriculum; }
