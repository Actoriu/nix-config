{ pkgs
, ...
}: {
  mysources = prev.callPackage (import ./_sources/generated.nix) { };

  hinalist = prev.callPackage ./dnsmasq-china-list {
    format = "raw";
  };

  chinalist-dnsmasq = prev.callPackage ./dnsmasq-china-list {
    format = "dnsmasq";
    enable-nftset = true;
  };

  chinalist-smartdns = prev.callPackage ./dnsmasq-china-list {
    format = "smartdns";
    upstream-dns = "china";
  };

  gfwlist = prev.callPackage ./gfwlist {
    format = "raw";
  };

  gfwlist-dnsmasq = prev.callPackage ./gfwlist {
    format = "dnsmasq";
    enable-nftset = true;
  };

  gfwlist-smartdns = prev.callPackage ./gfwlist {
    format = "smartdns";
  };
}
