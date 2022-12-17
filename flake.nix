{
  description = "Mimsa";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        compilerVersion = "ghc925";
        
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            "${compilerVersion}" =
              pkgs.haskell.packages."${compilerVersion}".override {
                overrides = self: super: {
                  # On aarch64-darwin, this creates a cycle for some reason; didn't look too much into it.
                  ghcid = pkgs.haskell.lib.overrideCabal super.ghcid (drv: { enableSeparateBinOutput = false; });
                };

              };
          };
        };

        haskellPackages = haskell.packages.${compilerVersion};

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "mimsa";
      in {
        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            ghcid
            cabal-install
          ];

          # put clang_14 on the path
          #shellHook = with pkgs; ''
          #  export PATH="${clang_14}/bin:$PATH"
          #'';

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}


