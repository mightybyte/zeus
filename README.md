# Zeus CI Server

## Introduction

Zeus is a CI server. You can use it to CI anything, but it is designed for the
purpose of doing CI with Nix projects.

Mainstream CI providers are not well suited for Nix projects. Because they run
on ephemeral containers that are created fresh each time you build, you have to
either install all your dependencies on every build or create your own image to
build from. Since Nix manages ALL dependencies from the lowest level system
library all the way up, this means copying a lot of data. Even if you have
everything cached, this still makes builds slower. If you don't have perfect
caching, it's even worse.

After trying many different providers we concluded that you really need
dedicated build machines for Nix projects. This allows you to get blazing fast
builds because all the build artifacts are kept in the build machine's nix store
across builds and you get perfect dependency caching out of the box.

Zeus makes it drop dead simple to get blazing fast CI for Nix projects.

## Installing

### Deploying to a Remote Server

The easiest way to deploy Zeus is with
[Obelisk](https://github.com/obsidiansystems/obelisk). To deploy with Obelisk,
you will need to have NixOS installed on the computer you are deploying to. You
also need to deploy from another Linux computer. You cannot use obelisk to
deploy from macOS or Windows.

If you already have the Reflex / Obelisk nix cache set up or if you don't mind
waiting longer for it to build, you can install Obelisk with this command:

```shell
nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command
```

Otherwise you may want to refer to the Obelisk documentation here:

https://github.com/obsidiansystems/obelisk#installing-obelisk

Once you have Obelisk installed, run the following commands. Be sure to
substitute correct values for your setup on the `ob deploy init` command. You
need to put your real email address so you can renew the LetsEncrypt certs when
they expire.

```shell
git clone https://gitlab.com/mightybyte/zeus.git
cd zeus
mkdir ob-deploy-info
ob deploy init ob-deploy-info --ssh-key path/to/ssh-key.pem  --hostname ci.example.com --route https://ci.example.com --admin-email you@example.com
cd ob-deploy-info
ob deploy push
```

Zeus CI should now be running on your server! Point your browser at
https://ci.example.com and you're off to the races.

### Running the Server Locally

```shell
git clone https://gitlab.com/mightybyte/zeus.git
cd zeus
nix-build -A exe -o result-exe
ln -s result-exe/frontend.jsexe.assets .
ln -s result-exe/static.assets .
result-exe/backend +RTS -N
```
