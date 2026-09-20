{
  description = "my python project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-python.url = "github:cachix/nixpkgs-python";
  };

  outputs = { self, nixpkgs, flake-utils, nixpkgs-python }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        pythonVersion = "3.14";
        python314 = nixpkgs-python.packages.${system}.${pythonVersion};
      in
        {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [ python314 git basedpyright python314Packages.ruff python314Packages.debugpy python314Packages.uv neo4j ];

            shellHook = ''
              if [ ! -d ".venv/" ]; then
                uv venv --no-managed-python --python "$(command -v python3)"
              fi

              source .venv/bin/activate
            '';
          };
        });
}
