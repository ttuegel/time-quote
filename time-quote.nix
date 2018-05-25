{ mkDerivation, base, stdenv, template-haskell, time }:
mkDerivation {
  pname = "time-quote";
  version = "1.9.0.0";
  src = ./.;
  libraryHaskellDepends = [ base template-haskell time ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}