{
  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux = with import nixpkgs { system = "x86_64-linux"; };
      mkShell {
        buildInputs = [ cargo rustc rustfmt rust-analyzer cargo-watch ];
        RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
        _PATH = "./tools";
      };
  };
}
