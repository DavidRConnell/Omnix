{
  description = "Omni exporter for Org files";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      tex = pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-basic latex-bin latexmk luatex fontspec koma-script luatex85

          # Formatting
          cleveref glossaries siunitx

          # Tables and figures
          caption float;
      };
    in {
      devShells.${system}.default = pkgs.mkShellNoCC {
        packages = (with pkgs; [ gnumake tex libreoffice zip ]);
      };
    };
}
