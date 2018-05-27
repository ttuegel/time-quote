{ mkDerivation, base, doctest, doctest-discover, stdenv
, template-haskell, time
}:
mkDerivation {
  pname = "time-quote";
  version = "1.9.0.0";
  src = ./.;
  libraryHaskellDepends = [ base template-haskell time ];
  testHaskellDepends = [
    base doctest doctest-discover template-haskell time
  ];
  license = stdenv.lib.licenses.gpl3;
}
