# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  dnsmasq-china-list = {
    pname = "dnsmasq-china-list";
    version = "c094dfcd71141ec507b1c689f0c85a979dd1284d";
    src = fetchFromGitHub {
      owner = "felixonmars";
      repo = "dnsmasq-china-list";
      rev = "c094dfcd71141ec507b1c689f0c85a979dd1284d";
      fetchSubmodules = false;
      sha256 = "sha256-8TMQjG3ImwOXABQaftl7AgJ54UQXMYBq0+4KISgjm6Y=";
    };
    date = "2024-05-04";
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
    version = "f7e5b81120dc67e278d10cc652200e7f922bccf3";
    src = fetchurl {
      url = "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.4/zhwiki-20230823.dict.yaml";
      sha256 = "sha256-2cx+enR+2lK0o+pYoP8CQg3qd2+nBpQVZhDj4pEPQjU=";
    };
    date = "2024-04-26";
  };
  gfwlist = {
    pname = "gfwlist";
    version = "ebf195d7fd05af517454fa53fc9270d721ec1867";
    src = fetchFromGitHub {
      owner = "gfwlist";
      repo = "gfwlist";
      rev = "ebf195d7fd05af517454fa53fc9270d721ec1867";
      fetchSubmodules = false;
      sha256 = "sha256-3KJ9M1TWRq7cQ9LeQORQMfJqTo7Pncvn/VuffgPTG8c=";
    };
    date = "2024-04-18";
  };
}
