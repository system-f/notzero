{ mkDerivation, base, bifunctors, lens, mtl, semigroupoids
, semigroups, stdenv, transformers
}:
mkDerivation {
  pname = "notzero";
  version = "0.0.12";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors lens mtl semigroupoids semigroups transformers
  ];
  homepage = "https://github.com/qfpl/notzero";
  description = "A data type for representing numeric values, except zero";
  license = stdenv.lib.licenses.bsd3;
}
