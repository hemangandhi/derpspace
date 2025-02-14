# Nix packages version of "the set of things you need in your system to code
# this"

with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "cshell";
  buildInputs = [
    gcc
    gnumake
  ];
}
