{ stdenv, lib, mysources }:

stdenv.mkDerivation rec {
  inherit (mysources) pname version src;

  dontUnpack = true;
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    install -D -m644 $src $out/share/fcitx5/pinyin/dictionaries/moegirl.dict
  '';

  meta = with lib; {
    description = "Fcitx 5 pinyin dictionary of zh.moegirl.org.cn";
    homepage = "https://github.com/outloudvi/mw2fcitx";
    license = licenses.unlicense;
  };
}
