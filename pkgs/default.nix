{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  sources = callPackage (import ./_sources/generated.nix) {};
in {
  chinalist = callPackage ./dnsmasq-china-list {
    inherit sources;
    format = "raw";
  };

  chinalist-dnsmasq = callPackage ./dnsmasq-china-list {
    inherit sources;
    format = "dnsmasq";
    enable-nftset = true;
  };

  chinalist-smartdns = callPackage ./dnsmasq-china-list {
    inherit sources;
    format = "smartdns";
    upstream-dns = "china";
  };

  gfwlist = callPackage ./gfwlist {
    inherit sources;
    format = "raw";
  };

  gfwlist-dnsmasq = callPackage ./gfwlist {
    inherit sources;
    format = "dnsmasq";
    enable-nftset = true;
  };

  gfwlist-smartdns = callPackage ./gfwlist {
    inherit sources;
    format = "smartdns";
  };
}
