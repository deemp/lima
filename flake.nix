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

            lima = "lima";

            override = {
              overrides = self: super: {
                doctest-parallel = super.doctest-parallel_0_3_0_1;
                ${lima} = super.callCabal2nix lima ./lima { };
              };
            };

            ghcVersion = "928";

            inherit (toolsGHC {
              version = ghcVersion;
              inherit override;
            }) haskellPackages;

            packages = {
              default = haskellPackages.${lima};
              sdist = (haskellPackages.buildFromCabalSdist haskellPackages.${lima}).overrideAttrs (_: { pname = "lima-sdist"; });
            };
          in
          {
            inherit packages;
            params = { inherit override ghcVersion lima; };
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
