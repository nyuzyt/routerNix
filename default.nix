{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
	owner = "NixOS";
	repo = "nixpkgs";
	rev = "09218fe5de4e85d182a1e55386996cd284ad4049";
	sha256 = "1mmrnyp80jfvqdsal9728h8y3pmyx5snvgls0zfmbjl4qm3m8f76";
  }){}
}:
let
  inherit (pkgs) runCommand closurecompiler;
    inherit (pkgs.haskell.packages) ghc844;
    ghc = ghc844.override (oldAttrs: {
      overrides = with pkgs.haskell.lib; self: super: {
      };
    });
    ghcjs = pkgs.haskell.packages.ghcjs.override (oldAttrs: {
      overrides = with pkgs.haskell.lib; self: super: {
        jsaddle-warp = dontCheck (super.callPackage ./nix/jsaddle-warp-ghcjs.nix {});
        miso = dontCheck super.miso;
        aeson = dontCheck super.aeson;
        http-types = dontCheck super.http-types;
        doctest = dontCheck super.doctest;
        tasty-quickcheck = dontCheck super.tasty-quickcheck;
        scientific = dontCheck super.scientific;
        directory-tree = dontCheck super.directory-tree;
        webdriver = dontCheck super.webdriver;
        servant = dontCheck super.servant;
      };
    });

in ghcjs.callPackage ./app.nix {
}