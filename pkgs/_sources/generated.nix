# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "e0639127398a803078e2c70b21c71188d4ee25af";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "e0639127398a803078e2c70b21c71188d4ee25af";
      fetchSubmodules = false;
      sha256 = "sha256-jzf46GXl4GzDfvn3QaSt9c8fc6uBzFT26Yxi62sFABM=";
    };
    date = "2023-11-03";
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
    version = "45a8103afd12a1decca35dce2f076f4f5b8cdc59";
    src = fetchurl {
      url = "https://github.com/outloudvi/mw2fcitx/releases/download/20230414/moegirl.dict.yaml";
      sha256 = "sha256-0hDjF1/Gt+OU6/YEY+t1efIRWpqPrKIiaMRBQ0zEWJg=";
    };
    date = "2022-09-24";
  };
  fcitx5-pinyin-zhwiki = {
    pname = "fcitx5-pinyin-zhwiki";
    version = "78a43785d7e8fade2955f2814fc5ad0cd18e4b6d";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2023-10-16";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "9fb0673a5f199b028f3b4f9cb3c7400bf0f575aa";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "9fb0673a5f199b028f3b4f9cb3c7400bf0f575aa";
      fetchSubmodules = false;
      sha256 = "sha256-2z/X3UeiRy/+c9VXMJ16cA01y85hOb6mfU+oeX15HRk=";
    };
    date = "2023-09-17";
  };
}
