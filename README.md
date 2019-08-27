# Zeus CI Server

## Introduction

Zeus is a no-fuss production quality CI server for Nix projects.

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
across builds. You get perfect dependency caching out of the box with minimal
rebuilding or transferring cached data over the network.

### Features

Zeus makes it drop dead simple to get blazing fast CI for Nix projects **today** by
providing the following features:

* Automatically sets up webhooks for both GitHub and GitLab repositories.
* All builds made available from the Zeus machine as a Nix binary cache so all
  your infrastructure from development to deployment rebuilds as little as
  possible.
* Support for pushing build outputs to a Nix cache hosted on S3.
* Organizations with closed source software can limit server and cache access to
  specific IP addresses / subnets.

### Roadmap

The following features are high priority and planned for the very near future:

* Ability to distribute builds to multiple build slaves
* Proper support for merge requests and control of GitHub/GitLab status lights
  with a safe solution to the problem of anyone on the internet having arbitrary
  code execution on non-ephemeral build servers.

Other tasks

* Fully automated DB migrations

## Installing

### Deploying to a Remote Server with Obelisk

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

### Running Zeus Without Obelisk

```shell
git clone https://gitlab.com/mightybyte/zeus.git
cd zeus
nix-build -A exe -o result-exe
ln -s result-exe/frontend.jsexe.assets .
ln -s result-exe/static.assets .
result-exe/backend +RTS -N
```

This runs the Zeus server on port 8000. Point your browser at
http://localhost:8000 and you should see the Zeus web UI.

In order for Zeus to work this way, the server it is running on needs to be
reachable from GitHub or GitLab and you need to tell the frontend what that
address is. To do that, put the address in the file `config/common/route` as
follows:

```
http://ci.example.com:8000
```

### Running Zeus Locally

If you are running Zeus behind NAT or in some other situation where it's not
reachable from the internet (perhaps if you're contributing to Zeus and testing
on your local machine for instance), you can get it to work by setting up a
reverse SSH tunnel to a machine that does have a public address. For example, if
you are running it as described above and you have SSH access to a machine at
`ci.example.com` with a public IP, you can set up an SSH tunnel as follows:

```
ssh -R '*:8000:localhost:8000' alice@ci.example.com
```

You need to have this SSH tunnel open when you create the Repo in the Zeus UI,
and also have it open whenever you want pushes to the repository to trigger
builds on Zeus.

With this setup you need to set `config/common/route` to this:

```
http://localhost:8000
```

This is because the frontend is running locally and needs to open websocket
connections to `localhost` to connect to the server.

The astute reader will notice a problem here. This is the correct address for
the frontend to use for its websocket connection, but it is not the correct
address for GitHub/GitLab to use to connect to Zeus. If you have a public IP
these two addresses are the same, but if you are running locally these two
addresses are different. In this situation you can tell Zeus the public address
that GitHub/GitLab should use by specifying `webhookBaseUrl` in the
`config/backend/settings` as described in the "Configuring the Backend" section
below.

## Setup

### Configuring the Backend

The Zeus backend has a few configuration options that you can specify in the
file `config/backend/settings.json` on the server. This file should contain
server settings in JSON format. Here is an example settings file:


```json
{
  "webhookBaseUrl": "http://ci.example.com:8000",
  "ipWhitelist": [
    "52.201.220.249/32",
    "176.9.155.62/32"
  ]
}
```

If you're deploying with Obelisk as described above, you should create this file
in `ob-deploy-info/config/backend/settings.json` after you do `ob deploy init`
but before `ob deploy push`.

`webhookBaseUrl` is an optional field described above in the "Running Zeus
Locally" section. `ipWhitelist` is a list of CIDR blocks describing IP addresses
that are allowed to connect to the server. If the list is empty, then the server
will be accessible from anywhere. This feature is primarily intended as a quick
and dirty way to provide access control until we get proper authentication and
access control implemented.

### Setting up an Account

Once you have gotten Zeus running through one of the above methods, you need to
connect it to your GitHub/GitLab account and set up repositories that you want
it to build. Point your browser at the frontend address from above
(http://localhost:8000, etc). The first thing you need to do is set up an
account. Click the "Accounts" tab, and then click "Connect Account", then
type the name of your account.

After you do this you need to create a personal access token. You can do
this as follows:

### GitHub

1. Go to https://github.com/settings/tokens
2. Click "Generate new token"
3. Type something descriptive in the Note field like "Zeus CI"
4. Check "repo" and all its sub-items
5. Check "admin:repo_hook" and all its sub-items
6. Click "Generate token"

### GitLab

1. Go to https://gitlab.com/profile/personal_access_tokens
2. Type something descriptive in the Name field like "Zeus CI"
3. Check "api", "read_user", and "read_repository"
4. Click "Create personal access token"

Once you've done this the access token will be displayed and you can copy it to
your clipboard. Then go back to Zeus and paste this into the "Access Token"
field. Then select the appropriate value for "Provider" and click "Connect
Account".

### Setting up a repo

Now that you have an account connected, click on the "Repos" tab and click "Add
Repository". Select the appropriate account from the "Access Account" dropdown.
Here are some examples of how to set the "Repo Namespace" and "Repo Name" fields.

If your repo URL is "https://github.com/alice/my-repo", the namespace will be
"alice" and the name will be "my-repo". The values would be the same for GitHub
or GitLab.

If your repo URL is "https://gitlab.com/acme-corp/frontend-team/foo-frontend",
the namespace will be "acme-corp/frontend-team" and the name will be
"foo-frontend".

Fill out the rest of the form as appropriate, then click "Add Repo" and you
should be good to go! Zeus will now run a build every time someone pushes to
your repo.

### Setting up as S3 cache

1. Create an Amazon S3 bucket to store your cache
2. Go to the bucket's "Properties" tab, click on "Static website hosting", and
   check "Use this bucket to host a website".
3. Go to "Permissions" -> "Block public access", click "Edit", and uncheck all
   the boxes.
4. In "Permissions" -> "Bucket Policy", set the following policy:

```
{
    "Version": "2012-10-17",
    "Id": "DirectReads",
    "Statement": [
        {
            "Sid": "AllowDirectReads",
            "Effect": "Allow",
            "Principal": "*",
            "Action": [
                "s3:ListBucket",
                "s3:GetObject"
            ],
            "Resource": [
                "arn:aws:s3:::my-nix-cache-bucket-name",
                "arn:aws:s3:::my-nix-cache-bucket-name/*"
            ]
        }
    ]
}
```

...substituting `my-nix-cache-bucket-name` with the name of your bucket.

5. Go to the "Caches" tab, click "Add Cache", fill in the form with information
   for your cache, and click "Connect Cache".
6. Create a new repo in the "Repos" tab and select the S3 cache you just
   created.
7. To enable this cache on your machine, edit `/etc/nix/nix.conf` and add the
   Zeus Cache Public Key (viewable in the Settings tab) to
   `trusted-public-keys`. Then add one of the following addresses to the
   `substituters` line:

* `s3://my-nix-cache-bucket-name`
* `http://my-nix-cache.example.com`
* `https://my-nix-cache.example.com`

If you want anyone to use the `s3://` prefix, you will have to include the
`s3:ListBucket` action as shown in the above policy. But if you want to use this
cache to serve private code, you definitely don't want to allow people to list
the objects in the bucket because then they will be able to download anything in
your cache. If you use the `http://` or `https://` prefix, then you can remove
the `ListBucket` action and it will still work. In that case you have to be sure
to set up a domain name to point to the S3 bucket.

NOTE: The first time you do a build that pushes to your S3 cache, it will
probably take several hours to upload the full transitive closure of all the
dependencies. After that first time, it will be much faster because most of the
dependencies will already be there.

### Migrations

Fully automated migration handling isn't planned until core CI features have
been implemented. In the meantime we will put all the necessary SQL code for
migrations in [migrations.md](migrations.md). If you encounter a migration that
cannot be automated, the backend server will fail to start. The easiest way to
solve this is to delete `zeus.db` and restart the server. Or, if you want to
preserve your existing Zeus database, run the migrations from `migrations.db`
manually.
