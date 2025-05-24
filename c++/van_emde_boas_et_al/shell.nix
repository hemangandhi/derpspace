let
  nixpkgs = import <nixpkgs> {};
  cppFlags = [
    "-std=c++20"
    "-Wall"
    "-Wextra"
    "-pedantic"
  ];
  nvim = import ../../nvim.nix ( rec {
    nixPkgs = nixpkgs;
    neovim = nixpkgs.neovim;
    customPackages = with nixpkgs.vimPlugins; {
      start = [
        coc-nvim
        coc-git
        vim-lsp
        vim-nix
        nvim-treesitter
        vim-clang-format
      ];
    };
  }); in
nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    # Build system
    gcc
    cmake
    # Testing framework
    gtest
    # Debugger
    # gdb
    # Editor
    nvim
    # coc needs this
    nixpkgs.nodejs
  ];

  # Environment variables for your shell
  shellHook = ''
    export CXXFLAGS="${nixpkgs.lib.concatStringsSep " " cppFlags}"
    export CFLAGS="${nixpkgs.lib.concatStringsSep " " cppFlags}"
    echo "Use 'cmake .' to configure, 'cmake --build .' to build, and 'ctest' to run tests."
  '';
}
