{ mkDerivation, base, miso, servant, stdenv }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso servant ];
  description = "First miso app";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
