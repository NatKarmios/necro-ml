{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  packages = with pkgs; [
    opam
    gnumake
  ];
  shellHook = ''
    eval $(opam env)
  '';
}
