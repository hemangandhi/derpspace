let
  nixpkgs = import <nixpkgs> {};
  nvim = import ./nvim.nix ( rec {
    nixPkgs = nixpkgs;
    neovim = nixpkgs.neovim;
    customPackages = with nixpkgs.vimPlugins; {
      start = [
        plenary-nvim
        telescope-nvim
        lean-nvim
        vimtex
      ];
    };
    extraRC = ''
      let &runtimepath = &runtimepath . ',/home/heman/dot-files/nix-envs/nvim-lua-inits/lean-init'
      " VimTex confs
      let g:vimtex_view_method = 'zathura'
      let g:vimtex_compiler_method = 'tectonic'
    '';
  }); in
nixpkgs.stdenv.mkDerivation {
  name = "lean-env";
  buildInputs = [
    # editor
    nvim
    nixpkgs.nodejs
    nixpkgs.tectonic
    # PL
    nixpkgs.lean4
    nixpkgs.texliveTeTeX
    # convenience
    nixpkgs.xclip
    nixpkgs.zathura
  ];
}
