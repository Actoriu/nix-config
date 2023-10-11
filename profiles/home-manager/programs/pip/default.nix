{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    pip = {
      enable = true;
      pipconf = ''
        [global]
        index-url = https://pypi.tuna.tsinghua.edu.cn/simple
        extra-index-url = https://mirror.sjtu.edu.cn/pypi/web/simple https://mirrors.bfsu.edu.cn/pypi/web/simple
      '';
    };
  };
}
