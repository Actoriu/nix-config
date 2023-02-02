{
  stdenv,
  lib,
  format ? "raw",
  upstream-dns ? "8.8.8.8",
  enable-ipset ? false,
  enable-nftset ? false,
  sources,
  ...
}:
stdenv.mkDerivation rec {
  inherit (sources) pname version src;

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
        cp gfwlist.txt gfwlist.domains.txt build/
        ;;
      dnsmasq)
        awk '{print "server=/" $0 "/${upstream-dns}"}' \
            gfwlist.domains.txt >build/gfwlist.dnsmasq.conf
        ;;
      smartdns)
        awk '{print "nameserver /" $0 "/gfwlist"}' \
            gfwlist.domains.txt >build/gfwlist.smartdns.conf
        ;;
      esac
    ''
    + (lib.optionalString enable-ipset ''
      case ${format} in
      dnsmasq)
        awk '{print "ipset=/" $0 "/gfwlist4,gfwlist6"}' \
            gfwlist.domains.txt >build/gfwlist.dnsmasq.ipset.conf
        ;;
      smartdns)
        awk '{print "ipset /" $0 "/#4:gfwlist4,#6:gfwlist6"}' \
            gfwlist.domains.txt >build/gfwlist.smartdns.ipset.conf
        ;;
      esac
    '')
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
