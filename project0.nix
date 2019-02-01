{ mkDerivation, base, jsaddle-warp, miso, stdenv }:
mkDerivation {
  pname = "routerNix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base jsaddle-warp miso ];
  license = stdenv.lib.licenses.bsd3;
}
