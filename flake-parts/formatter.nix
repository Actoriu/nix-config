{...}: {
  perSystem = {
    config,
    lib,
    pkgs,
    ...
  }: {
    formatter = config.treefmt.build.wrapper;
  };
}
