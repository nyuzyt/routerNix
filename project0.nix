{ mkDerivation, aeson, attoparsec, base, base-compat, blaze-html
, blaze-markup, bytestring, directory, http-media, http-types
, lucid, miso, mtl, servant, servant-server, stdenv
, string-conversions, time, wai, warp
}:
mkDerivation {
  pname = "routerNix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base base-compat blaze-html blaze-markup
    bytestring directory http-media http-types lucid miso mtl servant
    servant-server string-conversions time wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
