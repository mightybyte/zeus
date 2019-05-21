{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
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
    backend = addBuildDepend super.backend pkgs.git;
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
  };
})
