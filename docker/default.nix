## docker/default.nix

{ system ? "x86_64-linux", pkgs ? import <nixpkgs> { inherit system; } }:

let everything = import ../default.nix {};
    mimsa-app = everything.mimsa.components.exes.mimsa; 

in

with pkgs;

dockerTools.buildImage {
  name = "danieljamesharvey/mimsa-api";
  tag = "latest";
  contents = [ mimsa-app ];

  fromImage = dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
      sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };

  config = {
    Cmd = [ "${mimsa-app}/bin/mimsa" "server" ];
    ExposedPorts = {
      "6000/tcp" = {};
    };
  };
}
