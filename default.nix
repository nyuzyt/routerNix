let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          project0 =
            haskellPackagesNew.callPackage ./project0.nix { };

        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project0 = pkgs.haskellPackages.callPackage ./project0.nix { };
    }