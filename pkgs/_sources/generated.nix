# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "d3edf9a8907517acee0db119c6ac80e3a2d805a3";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "d3edf9a8907517acee0db119c6ac80e3a2d805a3";
      fetchSubmodules = false;
      sha256 = "sha256-jbaFKmAriRH+kKrTnRT2FDm8nHXge9GIj4yaB2uvUWE=";
    };
    date = "2024-11-02";
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
    version = "8d1a0262b894ca194014308f406c19d0153b5b13";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2024-09-16";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "d7f16cea44e06c6bce3c54867b25d5e48a1f4c63";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "d7f16cea44e06c6bce3c54867b25d5e48a1f4c63";
      fetchSubmodules = false;
      sha256 = "sha256-u4G8OFdkvvflBvNRxmaxd1sq/rCOKxvRQM6HMIm7oBk=";
    };
    date = "2024-10-07";
  };
}
