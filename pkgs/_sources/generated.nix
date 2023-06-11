# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "ef8ecdae07526dffb7e89ca0eef3caf6df74e409";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "ef8ecdae07526dffb7e89ca0eef3caf6df74e409";
      fetchSubmodules = false;
      sha256 = "sha256-rR7EQMSc44BIpvgRQ15sU1JDeiwUglk5Aa+O8DOraDs=";
    };
    date = "2023-06-09";
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
    version = "7816306f27cdbb79afd6319ad8fe8970708db4b0";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230329.dict.yaml";
      sha256 = "sha256-0B02FGISHqvrBUGkERdplhrx8zo6SV9hG2qRbSUqqd0=";
    };
    date = "2023-06-05";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "7923ab2ccb112c0416be2a8d86cbae2fcf0e9b68";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "7923ab2ccb112c0416be2a8d86cbae2fcf0e9b68";
      fetchSubmodules = false;
      sha256 = "sha256-z9lKwhIpPG3RwYXWvqLO8BaI0KJ3on1fPThZBpnNrEs=";
    };
    date = "2023-05-19";
  };
}
