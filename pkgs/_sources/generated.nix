# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "14e4c9ce93433e3db5ee26d64567b247bb173f40";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "14e4c9ce93433e3db5ee26d64567b247bb173f40";
      fetchSubmodules = false;
      sha256 = "sha256-KTG4rR0OtMfORNm+OZCY2tpnb/MI2cQIwV+Y5lvOC4U=";
    };
    date = "2024-04-04";
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
    version = "734dc029c5e3b872f250de346fa5708b06220cc0";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2024-02-10";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "340f965d9c4a31ded06e3079dff36eafdc841717";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "340f965d9c4a31ded06e3079dff36eafdc841717";
      fetchSubmodules = false;
      sha256 = "sha256-5XBzkarZxRWhmVj8+SBGA9OWdUYASFCJcEzk7jZcHN0=";
    };
    date = "2024-03-28";
  };
}
