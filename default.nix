{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
let

  origObelisk = import ./.obelisk/impl {
    inherit system iosSdkVersion;
  };
  opkgs = origObelisk.reflex-platform.nixpkgs;
  ignorePaths =
    [ ".git" "tags" "TAGS" "README.md" "dist" "dist-newstyle"
      "frontend.jsexe.assets" "static.assets" "result-exe"
      "zeus-access-token" "zeus-cache-key.pub"
      "zeus-cache-key.sec" "zeus.db" "migrations.md"
    ];
  nix-thunk = import ./deps/nix-thunk {};
  beam-src = nix-thunk.thunkSource ./deps/beam;


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
        path = [
          opkgs.awscli
          opkgs.git
          opkgs.gnutar
          opkgs.gzip
          opkgs.nix
        ];
        script = ''
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} >>backend.output 2>&1 </dev/null
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
    path = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) ignorePaths ||
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

newObelisk.project ./. ({ pkgs, hackGet, ... }: {
  packages = {
    github = hackGet ./deps/github;
    reflex-dom-contrib = hackGet ./deps/reflex-dom-contrib;
  };
  overrides = self: super: with pkgs.haskell.lib;
  let callHackageDirect = {pkg, ver, sha256}@args:
        let pkgver = "${pkg}-${ver}";
        in self.callCabal2nix pkg (pkgs.fetchzip {
             url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
             inherit sha256;
           }) {};
      semantic-reflex-src = pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "1ba4ee0124135817d92ea2bea8bde720e94a2612";
        sha256 = "0mcbxxbysbqcffik722h8lxl69cncsvfmmlij9m61djaw9zwvrp1";
      };
      gargoylePkgs = import ./deps/gargoyle { haskellPackages = self; };
  in {
    inherit (gargoylePkgs) gargoyle gargoyle-postgresql gargoyle-postgresql-nix gargoyle-postgresql-connect;
    amazonka = doJailbreak (dontCheck (callHackageDirect {
      pkg = "amazonka";
      ver = "1.6.1";
      sha256 = "0plph5sv8k7hm6cbvj0i12g06xvjm83qz9f1j5aviz6j4jp2wpj6";
    }));
    amazonka-core = doJailbreak (dontCheck (callHackageDirect {
      pkg = "amazonka-core";
      ver = "1.6.1";
      sha256 = "1q5prw7hwgg4v0dr3g97kmr2caq17xn9ixsdz7i5yr2p3ad943dq";
    }));
    amazonka-s3 = dontCheck (callHackageDirect {
      pkg = "amazonka-s3";
      ver = "1.6.1";
      sha256 = "1h12xs40zbihz714pq6il8k733xsba4jzkjwrdzvfwms0hpraix4";
    });
    backend = overrideCabal super.backend (drv: {
      executableSystemDepends = drv.executableSystemDepends or [] ++ [
        pkgs.awscli
        pkgs.git
        pkgs.gnutar
        pkgs.gzip
        pkgs.nix
        pkgs.which
      ];
    });
#    base32-bytestring = (self.callCabal2nix "base32-bytestring" (pkgs.fetchFromGitHub {
#        owner = "FilWisher";
#        repo = "base32-bytestring";
#        rev = "0c4790ba150a35f7d0d56fe7262ccbe8407c2471";
#        sha256 = "1y0qifp8za9s8dzsflw51wyacpjwx4b8p0qpa4xxv46lc2c2gl6i";
#    }) {});
#
#    # aeson = dontCheck (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
#    #     owner = "bos";
#    #     repo = "aeson";
#    #     rev = "378ff1483876d794fc33adb70e4b69a089a1b841";
#    #     sha256 = "06wdwlxa6l5nzkpf7w5sqj10rnxbqd85d9v3j6567n5rc1cyy83c";
#    # }) {});
#    barbies = dontCheck (self.callCabal2nix "barbies" (pkgs.fetchFromGitHub {
#        owner = "jcpetruzza";
#        repo = "barbies";
#        rev = "3e50449afcc7c094657df86e82f8b77a2ab0aa95";
#        sha256 = "1yaln3xisqacw0arxmclncay9a4xj2i6fpacjnpdaigxakl9xdwv";
#    }) {});

    beam-automigrate = self.callHackageDirect {
      pkg = "beam-automigrate";
      ver = "0.1.2.0";
      sha256 = "1a70da15hb4nlpxhnsy1g89frbpf3kg3mwb4g9carj5izw1w1r1k";
    } {};

    beam-core = dontCheck (self.callCabal2nix "beam-core" "${beam-src}/beam-core" {});
    beam-migrate = doJailbreak (dontCheck (self.callCabal2nix "beam-migrate" "${beam-src}/beam-migrate" {}));
    beam-postgres = dontCheck (self.callCabal2nix "beam-postgres" "${beam-src}/beam-postgres" {});
    beam-sqlite = dontCheck (self.callCabal2nix "beam-sqlite" "${beam-src}/beam-sqlite" {});

#    binary-instances = dontCheck (callHackageDirect {
#      pkg = "binary-instances";
#      ver = "1.0.0.1";
#      sha256 = "0ngnzpfjmzijmj635pg6f034dlvbq2pds7k88bcd5mqpd9mp2hzp";
#    });
#    binary-orphans = dontCheck (callHackageDirect {
#      pkg = "binary-orphans";
#      ver = "1.0.1";
#      sha256 = "15p1wbfxwzja69s03qavs0nngymm80445ajfafi6r3x3ch76azm3";
#    });
     github = doJailbreak super.github;
#    heist = dontCheck (self.callCabal2nix "heist" (pkgs.fetchFromGitHub {
#        owner = "snapframework";
#        repo = "heist";
#        rev = "de802b0ed5055bd45cfed733524b4086c7e71660";
#        sha256 = "0gqvw9jp6pxg4pixrmlg7vlcicmhkw2cb39bb8lfw401yaq6ad4a";
#    }) {});
#    lens-aeson = dontCheck super.lens-aeson;
    rng-utils = dontCheck (callHackageDirect {
      pkg = "rng-utils";
      ver = "0.3.0";
      sha256 = "0h8h47zjp83k9xqjr6yjb7004siwn61njzjc1siwl6gm9ycpnm8w";
    });
    scrub = dontCheck (self.callCabal2nix "scrub" (pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "scrub";
        rev = "38a1e241e04e1e8ace266a2df51650492aaa1279";
        sha256 = "1as5kfryjxs2mv47wq6pkxq2m7jf6bihx4qrj1mvk31qyg5qghr2";
    }) {});
    semantic-reflex = dontHaddock (dontCheck
      (self.callCabal2nix "semantic-reflex" "${semantic-reflex-src}/semantic-reflex" {}));
#    shelly = dontCheck (self.callCabal2nix "snap-server" (pkgs.fetchFromGitHub {
#        owner = "yesodweb";
#        repo = "Shelly.hs";
#        rev = "cf2f48a298ce7a40da0283702a3d98d53db9027a";
#        sha256 = "14m3zp4f2n14chl4d0mb1n8i8kgx3x504h28zpjcvp27ffrxr1cl";
#    }) {});
#    snap-server = dontCheck (self.callCabal2nix "snap-server" (pkgs.fetchFromGitHub {
#        owner = "snapframework";
#        repo = "snap-server";
#        rev = "dad24ba290126b1b93da32ef6019393329b54ed3";
#        sha256 = "0fzbvysq6qkbjd39bphbirzd2xaalm3jaxrs91g04ya17nqdaz1i";
#    }) {});
#    streaming-lzma = dontCheck (self.callCabal2nix "streaming-lzma" (pkgs.fetchFromGitHub {
#        owner = "haskell-hvr";
#        repo = "streaming-lzma";
#        rev = "ec8cb2f935ee4f3217c6939684103ba1a6bc4ad1";
#        sha256 = "1w77v9isv6rmajg4py4ry7475d3xjs7471dfaf6bglbwphm0dj8b";
#    }) {});
#    time-compat = doJailbreak (dontCheck (callHackageDirect {
#      pkg = "time-compat";
#      ver = "1.9.3";
#      sha256 = "1r0g0j3zjw2abvaxnn73nrvbzdq0azlw7kgpi5zdvnx7lv873awg";
#    }));
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2";
      sha256 = "1g795yq36n7c6ycs7c0799c3cw78ad0cya6lj4x08m0xnfx98znn";
    } {};
    zeus = addBuildDepends super.zeus [ pkgs.git ];

  };
  shellToolOverrides = ghc: super: {
    inherit (pkgs) git;
    inherit (pkgs) nix;
    inherit (pkgs) which;
    inherit (ghc) hlint;
  };
})
