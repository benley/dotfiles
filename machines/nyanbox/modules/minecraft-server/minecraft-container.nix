{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "18.09";
  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
    declarative = true;
    serverProperties = {
      white-list = true;
      enforce-whitelist = true;
      enable-rcon = true;
      "rcon.password" = builtins.readFile ./minecraft-rcon-password.txt;
      "rcon.port" = 25575;
      enable-query = true;
      "query.port" = 25565;
    };
    whitelist = {
      benley = "da4b94d3-7686-4ddb-a837-92ff6f8a3370";
      RGBrazberry = "0204faba-10a9-49e1-ad92-87b6291ea0e3";
      davean = "b9893039-a9ff-4120-a72c-dd95ad378aa2";
      Neofuturus = "6d791fc9-aa30-4a5a-b393-b232930dfba3";
      UndeadGentleman = "9bb7258b-6229-41d1-a7b3-39a79cd9d9cb";
    };
    jvmOpts = builtins.concatStringsSep " " [
      # "-server"
      "-Xms8G"
      "-Xmx8G"
      "-XX:+UseG1GC"
      "-XX:+UnlockExperimentalVMOptions"
      "-XX:MaxGCPauseMillis=100"
      "-XX:+DisableExplicitGC"
      "-XX:TargetSurvivorRatio=90"
      "-XX:G1NewSizePercent=50"
      "-XX:G1MaxNewSizePercent=80"
      "-XX:G1MixedGCLiveThresholdPercent=35"
      "-XX:+AlwaysPreTouch"
      "-XX:+ParallelRefProcEnabled"
      # "-XX:UseSSE=4"
      # "-XX:+UseAES"
      # "-XX:+UseAESIntrinsics"
      # "-Dusing.aikars.flags=mcflags.emc.gs"
    ];
  };
}
