{ mkDerivation, base, deepseq, QuickCheck, random, stdenv, tasty
, tasty-hunit, tasty-quickcheck, unix
}:
mkDerivation {
  pname = "time";
  version = "1.9.1";
  sha256 = "479f5715c81b3eca6763c5859f665a2979bbeebacb23d478d5753f525d9f5bdb";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    base deepseq QuickCheck random tasty tasty-hunit tasty-quickcheck
    unix
  ];
  homepage = "https://github.com/haskell/time";
  description = "A time library";
  license = stdenv.lib.licenses.bsd3;
}
