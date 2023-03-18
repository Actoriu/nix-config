{
  lib,
  stdenv,
  sources,
  format ? "raw",
  upstream-dns ? "8.8.8.8",
  enable-nftset ? false,
}:
stdenv.mkDerivation rec {
  inherit (sources.gfwlist) pname version src;

  dontUnpack = true;
  dontBuild = true;
  dontConfigure = true;

  buildPhase =
    ''
      mkdir -p build
      base64 -d gfwlist.txt >gfwlist.raw.txt
      mv gfwlist.raw.txt gfwlist.txt
      awk '! /^\[|^!|^@|^\/|:|\/|%|*/ {
          if(match($0, /([0-9a-z-]+\.)+[a-z]+/)) {
              print substr($0, RSTART, RLENGTH)
          }
      }' gfwlist.txt | sort | uniq >gfwlist.domains.txt
      case ${format} in
      raw)
        cp -v gfwlist.txt gfwlist.domains.txt build/
        ;;
      dnsmasq)
        awk '{print "server=/" $0 "/${upstream-dns}"}' \
            gfwlist.domains.txt >build/gfwlist.dnsmasq.conf
        ;;
      routedns)
        awk '{print "." $0}' gfwlist.domains.txt >build/gfwlist.routedns.txt
        ;;
      esac
    ''
    + (lib.optionalString enable-nftset ''
      case ${format} in
      dnsmasq)
        awk '{print "nftset=/" $0 "/4#inet#filter#gfwlist4";
              print "nftset=/" $0 "/6#inet#filter#gfwlist6"}' \
            gfwlist.domains.txt >build/gfwlist.dnsmasq.nftset.conf
        ;;
      esac
    '');

  installPhase = ''
    mkdir -p $out
    cp -rv build/* $out
  '';

  meta = with lib; {
    description = "gfwlist";
    homepage = "https://github.com/gfwlist/gfwlist";
    license = licenses.lgpl21;
  };
}
