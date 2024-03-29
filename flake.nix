{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let makeFlake = inputs.flakes.makeFlake; in
    makeFlake {
      inputs = {
        inherit (inputs.flakes.all)
          nixpkgs formatter codium drv-tools devshell
          flakes-tools workflows haskell-tools;
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix writeSettingsJSON mkCodium;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
          inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
          inherit (inputs.workflows.lib.${system}) writeWorkflow steps nixCI run stepsIf names os;
          inherit (inputs.drv-tools.lib.${system}) mkShellApps getExe;

          lima = "lima";

          override = {
            overrides = self: super: {
              doctest-parallel = super.doctest-parallel_0_3_0_1;
              ${lima} = super.callCabal2nix lima ./${lima} { };
            };
          };

          ghcVersion = "928";

          # Next, set the desired GHC version
          inherit (toolsGHC {
            inherit override;
            version = ghcVersion;
            packages = ps: [ ps.${lima} ];
          }) cabal ghc hls hpack fourmolu ghcid haskellPackages;

          tools = [ ghc cabal hls hpack ghcid fourmolu ];

          packages =
            let
              packages1 = mkShellApps { writeDocs.text = "${getExe cabal} test ${lima}:test:readme"; };
              packages2 = {
                default = haskellPackages.${lima};
                sdist = (haskellPackages.buildFromCabalSdist haskellPackages.${lima}).overrideAttrs (_: { pname = "lima-sdist"; });

                codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
                writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
                inherit (mkFlakesTools { dirs = [ "." ]; root = ./.; }) updateLocks pushToCachix format;
                writeWorkflows = writeWorkflow "CI"
                  (
                    nixCI {
                      jobArgs = {
                        doPushToCachix = true;
                        cacheNixArgs = {
                          linuxGCEnabled = true;
                          linuxMaxStoreSize = 5100000000;
                          macosGCEnabled = true;
                          macosMaxStoreSize = 5100000000;
                        };
                        doUpdateLocks = true;
                        doCommit = false;
                        doSaveFlakes = false;
                        steps = dir:
                          stepsIf ("${names.matrix.os} == '${os.ubuntu-22}'")
                            (
                              let writeDocsName = "Write docs"; in
                              [
                                {
                                  name = writeDocsName;
                                  run = run.nixScript {
                                    inherit dir;
                                    name = packages1.writeDocs.pname;
                                  };
                                }
                                {
                                  name = "Commit & Push changes";
                                  run = run.nix {
                                    doCommit = true;
                                    commitArgs.messages = [ (steps.format { }).name (steps.updateLocks { }).name writeDocsName ];
                                  };
                                }
                              ]
                            )
                          ++ [
                            {
                              name = "Build lima";
                              run = run.nix_ { doInstall = true; scripts = [ "" "sdist" ]; };
                            }
                          ];
                      };
                    }
                  );
              };
            in
            packages1 // packages2;

          devShells.default = mkShell {
            packages = tools;
            bash.extra = "export LC_ALL=C.UTF-8";
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
              ++ [
                {
                  name = "test";
                  category = "test";
                  help = "Build via `cabal`";
                  command = "cabal v1-test";
                }
              ];
          };
        in
        {
          inherit packages devShells;
          formatter = inputs.formatter.${system};
        };
    };

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
