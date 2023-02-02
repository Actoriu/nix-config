{pkgs ? null}: {
  sources = pkgs.callPackage (import ./_sources/generated.nix) {};

  hinalist = pkgs.callPackage ./dnsmasq-china-list {
    format = "raw";
  };

  chinalist-dnsmasq = pkgs.callPackage ./dnsmasq-china-list {
    format = "dnsmasq";
    enable-nftset = true;
  };

  chinalist-smartdns = pkgs.callPackage ./dnsmasq-china-list {
    format = "smartdns";
    upstream-dns = "china";
  };

  gfwlist = pkgs.callPackage ./gfwlist {
    format = "raw";
  };

  gfwlist-dnsmasq = pkgs.callPackage ./gfwlist {
    format = "dnsmasq";
    enable-nftset = true;
  };

  gfwlist-smartdns = pkgs.callPackage ./gfwlist {
    format = "smartdns";
  };
}
