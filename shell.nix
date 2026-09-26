{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  packages = with pkgs; [
    haskell.compiler.ghc910
    haskellPackages.stack
    haskellPackages.cabal-install
    # LSP server
    haskellPackages.haskell-language-server
  ];
}
