# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "6ce518f40347e474463f2a56b4ed33d5f1c8464b";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "6ce518f40347e474463f2a56b4ed33d5f1c8464b";
      fetchSubmodules = false;
      sha256 = "sha256-t6kZihsqcZgLVUrShTPL8OvrU/VHMeU0j0rX25BGG+I=";
    };
    date = "2024-07-17";
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
    version = "ca3f1659dee9e2049d59507cb1b6827d71bd888c";
    src = fetchurl {
      url = "https://github.com/outloudvi/mw2fcitx/releases/download/20230414/moegirl.dict.yaml";
      sha256 = "sha256-0hDjF1/Gt+OU6/YEY+t1efIRWpqPrKIiaMRBQ0zEWJg=";
    };
    date = "2024-07-25";
  };
  fcitx5-pinyin-zhwiki = {
    pname = "fcitx5-pinyin-zhwiki";
    version = "de4c18c43cacd8ba2353f8d960cb4014386b6bcc";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2024-07-22";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "795605222ed84d8771330f31c5de96124753b4a6";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "795605222ed84d8771330f31c5de96124753b4a6";
      fetchSubmodules = false;
      sha256 = "sha256-nFdV8kTzz6+NV915jnuqKyhHXEhrNALMvsbDd1s9zr0=";
    };
    date = "2024-07-20";
  };
}
