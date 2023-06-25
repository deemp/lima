{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      packageName = "lima";

      ghcVersion = "928";

      hpkgs = pkgs.haskell.packages."ghc${ghcVersion}";
      
      package = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix packageName ./. { }) (
        x: {
          testHaskellDepends = [
            hpkgs.doctest-parallel_0_3_0_1
          ] ++ (x.testHaskellDepends or [ ]);
        }
      );

      packages = {
        default = package;
        sdist = hpkgs.buildFromCabalSdist package;
      };
    in
    {
      inherit packages;
    });

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
