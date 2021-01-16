with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "analysis-env";
  buildInputs = [
    python37
    python37Packages.glob2
    python37Packages.nltk
    python37Packages.beautifulsoup4
    python37Packages.pandas
    python37Packages.matplotlib
  ];
}
