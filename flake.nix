{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils formatter;
          haskell-tools = flakes.language-tools.haskell;
          inherit flakes;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.haskell-tools.lib.${system}) toolsGHC;

            packageName = "lima";

            override = {
              overrides = self: super: {
                doctest-parallel = super.doctest-parallel_0_3_0_1;
                ${packageName} = super.callCabal2nix packageName ./. { };
              };
            };

            ghcVersion = "928";

            toolsGHC_ = (toolsGHC {
              version = ghcVersion;
              inherit override;
              packages = (ps: [ ps.${packageName} ]);
            });

            package = toolsGHC_.haskellPackages.${packageName};

            packages = {
              default = package;
              sdist = (toolsGHC_.haskellPackages.buildFromCabalSdist package).overrideAttrs (_: { pname = "lima-sdist"; });
            };
          in
          {
            inherit packages toolsGHC_;
          })
        // {
          inherit (inputs) formatter;
        };
    in
    outputs;

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
