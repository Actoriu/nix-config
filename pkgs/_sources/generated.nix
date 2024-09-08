# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "ce723f31847f8db6e7c981d629f82fb42fb2d48d";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "ce723f31847f8db6e7c981d629f82fb42fb2d48d";
      fetchSubmodules = false;
      sha256 = "sha256-5yvnBtBc0OK1o8fVUk3TCO1sohebXo01W51ny7lseQE=";
    };
    date = "2024-09-06";
  };
  fcitx5-material-color = {
    pname = "fcitx5-material-color";
    version = "2256feeae48dcc87f19a3cfe98f171862f8fcace";
    src = fetchFromGitHub {
      owner = "hosxy";
      repo = "Fcitx5-Material-Color";
      rev = "2256feeae48dcc87f19a3cfe98f171862f8fcace";
      fetchSubmodules = false;
      sha256 = "sha256-i9JHIJ+cHLTBZUNzj9Ujl3LIdkCllTWpO1Ta4OT1LTc=";
    };
    date = "2021-02-21";
  };
  fcitx5-pinyin-moegirl = {
    pname = "fcitx5-pinyin-moegirl";
    version = "f059e9fd244b6f232f38d63a9c660da43c8f9253";
    src = fetchurl {
      url = "https://github.com/outloudvi/mw2fcitx/releases/download/20230414/moegirl.dict.yaml";
      sha256 = "sha256-0hDjF1/Gt+OU6/YEY+t1efIRWpqPrKIiaMRBQ0zEWJg=";
    };
    date = "2024-08-28";
  };
  fcitx5-pinyin-zhwiki = {
    pname = "fcitx5-pinyin-zhwiki";
    version = "6f21114646fb20549ae6586ed071ddd122a9c756";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2024-08-12";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "62bbcec4e0d85e2ad3873ac68b37fd0ff7b0692f";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "62bbcec4e0d85e2ad3873ac68b37fd0ff7b0692f";
      fetchSubmodules = false;
      sha256 = "sha256-Rjm7+NNpBrxfWZRf7jVIMf1A2QCVYM/u1CzyfK1Lx8M=";
    };
    date = "2024-09-02";
  };
}
