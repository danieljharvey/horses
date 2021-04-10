let
  haskellNix =
    let
      # 2020-12-21
      commit = "1843d06f782205b2e97421ace6139a8a3ef1c149";
      sha256 = "0nb6qbz8nncjkhh51rs6wy6ff0hp59fhqma93l9vcxh14s5lg1va";
    in
    import
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
        inherit sha256;
      })
      { };
  pkgsSrc = haskellNix.sources.nixpkgs-2009;
  pkgsArgs = haskellNix.nixpkgsArgs;
  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      src = self.haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "mimsa";
      };
      compiler-nix-name = "ghc8102";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
