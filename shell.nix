{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.sudoku.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.sudoku.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
