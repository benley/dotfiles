{ pkgs, fetchgit, fetchFromGitHub }:

let buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix; in {
  ### Example:
  #
  # "vim-trailing-whitespace" = buildVimPlugin {
  #   name = "vim-trailing-whitespace";
  #   src = fetchgit {
  #     url = "https://github.com/bronson/vim-trailing-whitespace";
  #     rev = "d4ad27de051848e544e360482bdf076b154de0c1";
  #     sha256 = "594769a6f901407609b635a5041966456bfd91b13437169a4562857544e1dca3";
  #   };
  #   dependencies = [];
  # };
  "base16-vim" = buildVimPlugin {
    name = "base16-vim";
    src = fetchFromGitHub {
      owner = "chriskempson";
      repo = "base16-vim";
      rev = "c1c3e6ccb1a4cd4ea00162abb4ccece4a3e69d0b";
      sha256 = "0v4izy03aqarrympfn7yk7x2ybic8aplfigpikg8hb42f911z8n2";
    };
  };

  "dockerfile.vim" = buildVimPlugin {
    name = "dockerfile.vim";
    src = fetchFromGitHub {
      owner = "ekalinin";
      repo = "Dockerfile.vim";
      rev = "e686a686734ebcbb122e3285312586cebf84b8ea";
      sha256 = "135fq1c9s0k3n7aqwjj3jzsnxnd96r9xbxw2fr8hz0l8538c9k8b";
    };
  };

  "vim-terraform" = buildVimPlugin {
    name = "vim-terraform";
    src = fetchFromGitHub {
      owner = "hashivim";
      repo = "vim-terraform";
      rev = "6cece1f81a2cf3c724f8b6edbc464050308fe5b9";
      sha256 = "1kfdzs5q3jnqgp11vp8addky1940nr6nqvzv806zm7rjw21vmx25";
    };
  };

  "vim-virtualenv" = buildVimPlugin {
    name = "vim-virtualenv";
    src = fetchFromGitHub {
      owner = "jmcantrell";
      repo = "vim-virtualenv";
      rev = "85b14c7e3f7f0f0ea9cf2c7e010f4c1b44e9eaf1";
      sha256 = "13s9a9j5qqrlwcfq1pw5kip5zy9d65rzb0frdp2afq3v1nxj0h7w";
    };
  };
}
