{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
let

  origObelisk = import ./.obelisk/impl {
    inherit system iosSdkVersion;
  };
  opkgs = origObelisk.reflex-platform.nixpkgs;
  extraIgnores =
    [ "dist-newstyle" "frontend.jsexe.assets" "static.assets" "result-exe"
      "zeus-access-token" "zeus-cache-key.pub" "zeus-cache-key.sec" "zeus.db"
    ];


  myMkObeliskApp =
    { exe
    , routeHost
    , enableHttps
    , name ? "backend"
    , user ? name
    , group ? user
    , baseUrl ? "/"
    , internalPort ? 8000
    , backendArgs ? "--port=${toString internalPort}"
    , ...
    }: {...}: {
      services.nginx = {
        enable = true;
        virtualHosts."${routeHost}" = {
          enableACME = enableHttps;
          forceSSL = enableHttps;
          locations.${baseUrl} = {
            proxyPass = "http://localhost:" + toString internalPort;
            proxyWebsockets = true;
          };
        };
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        path = [ opkgs.gnutar ];
        script = ''
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} </dev/null
        '';
        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
        };
      };
      users = {
        users.${user} = {
          description = "${user} service";
          home = "/var/lib/${user}";
          createHome = true;
          isSystemUser = true;
          group = group;
        };
        groups.${group} = {};
      };
    };

  myServerModules = origObelisk.serverModules // {
    mkObeliskApp = myMkObeliskApp;
  };

  newObelisk = origObelisk // {
    path = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) ([".git" "tags" "TAGS" "dist"] ++ extraIgnores) ||
                                                builtins.match ".swp$" path)) ./.;
    serverModules = myServerModules;

    server = { exe, hostName, adminEmail, routeHost, enableHttps, version }@args:
      let
        nixos = import (opkgs.path + /nixos);
      in nixos {
        system = "x86_64-linux";
        configuration = {
          imports = [
            (origObelisk.serverModules.mkBaseEc2 args)
            (myMkObeliskApp args)
          ];
        };
      };


  };

in

newObelisk.project ./. ({ pkgs, ... }: {
  overrides = self: super: with pkgs.haskell.lib;
  let beam-src = pkgs.fetchFromGitHub {
        owner = "tathougies";
        repo = "beam";
        rev = "737b73c6ec1c6aac6386bf9592a02a91f34a9478";
        sha256 = "02xc4qgc7kb0rv8g9dq69p3p0d2psp6b4mzq444hsavnsw2wsn9y";
      };
      semantic-reflex-src = pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "a354fda1f34d06b72fd99dea1206606b5210ecdd";
        sha256 = "1li8w95ibq4xm717clz5wz23kdp15j9vrqb1kq64d5ld0fjx7ln0";
      };
  in {
    backend = overrideCabal super.backend (drv: {
      librarySystemDepends = drv.librarySystemDepends or [] ++ [
        pkgs.git
        pkgs.nix
        pkgs.gnutar
      ];
    });
    base32-bytestring = (self.callCabal2nix "base32-bytestring" (pkgs.fetchFromGitHub {
        owner = "FilWisher";
        repo = "base32-bytestring";
        rev = "0c4790ba150a35f7d0d56fe7262ccbe8407c2471";
        sha256 = "1y0qifp8za9s8dzsflw51wyacpjwx4b8p0qpa4xxv46lc2c2gl6i";
    }) {});

    # aeson = dontCheck (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
    #     owner = "bos";
    #     repo = "aeson";
    #     rev = "378ff1483876d794fc33adb70e4b69a089a1b841";
    #     sha256 = "06wdwlxa6l5nzkpf7w5sqj10rnxbqd85d9v3j6567n5rc1cyy83c";
    # }) {});
    barbies = dontCheck (self.callCabal2nix "barbies" (pkgs.fetchFromGitHub {
        owner = "jcpetruzza";
        repo = "barbies";
        rev = "3e50449afcc7c094657df86e82f8b77a2ab0aa95";
        sha256 = "1yaln3xisqacw0arxmclncay9a4xj2i6fpacjnpdaigxakl9xdwv";
    }) {});
    beam-core = dontCheck (self.callCabal2nix "beam-core" "${beam-src}/beam-core" {});
    beam-migrate = dontCheck (self.callCabal2nix "beam-migrate" "${beam-src}/beam-migrate" {});
    beam-sqlite = dontCheck (self.callCabal2nix "beam-sqlite" "${beam-src}/beam-sqlite" {});

    github = dontHaddock (doJailbreak (dontCheck (self.callCabal2nix "github" (pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "github";
        rev = "a337ff7c6b185f2ca275a8ecfdfa4f100f68925d";
        sha256 = "1npvqh8jjpgbygpsfwwr196mgkl0z1c92fbd51qlnir39lgrkljd";
    }) {})));
    heist = dontCheck (self.callCabal2nix "heist" (pkgs.fetchFromGitHub {
        owner = "snapframework";
        repo = "heist";
        rev = "de802b0ed5055bd45cfed733524b4086c7e71660";
        sha256 = "0gqvw9jp6pxg4pixrmlg7vlcicmhkw2cb39bb8lfw401yaq6ad4a";
    }) {});
    lens-aeson = dontCheck super.lens-aeson;
    reflex-dom-contrib = dontCheck (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "796a3f0fa1ff59cbad97c918983355b46c3b6aa0";
        sha256 = "0aqj7xm97mwxhhpcrx58bbg3hhn12jrzk13lf4zhpk2rrjw6yvmc";
    }) {});
    scrub = dontCheck (self.callCabal2nix "scrub" (pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "scrub";
        rev = "38a1e241e04e1e8ace266a2df51650492aaa1279";
        sha256 = "1as5kfryjxs2mv47wq6pkxq2m7jf6bihx4qrj1mvk31qyg5qghr2";
    }) {});
    semantic-reflex = dontHaddock (dontCheck
      (self.callCabal2nix "semantic-reflex" "${semantic-reflex-src}/semantic-reflex" {}));
    shelly = dontCheck (self.callCabal2nix "snap-server" (pkgs.fetchFromGitHub {
        owner = "yesodweb";
        repo = "Shelly.hs";
        rev = "cf2f48a298ce7a40da0283702a3d98d53db9027a";
        sha256 = "14m3zp4f2n14chl4d0mb1n8i8kgx3x504h28zpjcvp27ffrxr1cl";
    }) {});
    snap-server = dontCheck (self.callCabal2nix "snap-server" (pkgs.fetchFromGitHub {
        owner = "snapframework";
        repo = "snap-server";
        rev = "dad24ba290126b1b93da32ef6019393329b54ed3";
        sha256 = "0fzbvysq6qkbjd39bphbirzd2xaalm3jaxrs91g04ya17nqdaz1i";
    }) {});
    zeus = addBuildDepends super.zeus [ pkgs.git ];

  };
  shellToolOverrides = ghc: super: {
    inherit (pkgs) git;
    inherit (pkgs) nix;
    inherit (ghc) hlint;
  };
})
