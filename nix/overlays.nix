{ sources, compiler }:
[
  (final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (final: prev: {
    sudoku = import ./packages.nix { pkgs = prev; inherit compiler; };
  })
]
