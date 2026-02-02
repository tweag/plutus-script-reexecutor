{
  description = "plutus-script-reexecutor";

  inputs = {

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    flake-utils.url = "github:numtide/flake-utils";

    cardano-node.url = "github:tweag/cardano-node/fc02d1ef2126ffe5b62a6eda5a08a52347ebb6ef";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system: 
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [ 
      "https://cache.iog.io" 
      "https://cache.zw3rk.com" 
      "https://plutus-script-reexecutor.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "plutus-script-reexecutor.cachix.org-1:quy7tNPhVnfvx8oCH8Jji7Obj+hdyd09k3igdcIseh4="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
