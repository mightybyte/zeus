# Migrations

Haven't had time to put in place a fully automated migration system, so until
that happens will put migration code here.

### 2019-07-07 Migration for removing clone method column

```
ALTER TABLE "ciDb_repos" RENAME TO "ciDb_repos_old_0";
CREATE TABLE IF NOT EXISTS "ciDb_repos"("repo_id" INTEGER NOT NULL , "repo_accessAccount__connectedAccount_id" INTEGER NOT NULL , "repo_name" VARCHAR NOT NULL , "repo_namespace" VARCHAR NOT NULL , "repo_buildNixFile" VARCHAR NOT NULL , "repo_timeout" INTEGER NOT NULL , "repo_hookId" INTEGER NOT NULL , PRIMARY KEY("repo_id"));
INSERT INTO "ciDb_repos" SELECT "repo_id", "repo_accessAccount__connectedAccount_id", "repo_name", "repo_namespace", "repo_buildNixFile", "repo_timeout", "repo_hookId" FROM "ciDb_repos_old_0";
DROP TABLE ciDb_repos_old_0;
```

### 2019-08-12 Per-repository S3 caches and keep track of what has been uploaded to them

#### Added nullable binaryCache_id column
```
ALTER TABLE "ciDb_repos" RENAME TO "ciDb_repos_old_1";
CREATE TABLE IF NOT EXISTS "ciDb_repos"("repo_id" INTEGER NOT NULL , "repo_accessAccount__connectedAccount_id" INTEGER NOT NULL , "repo_name" VARCHAR NOT NULL , "repo_namespace" VARCHAR NOT NULL , "repo_buildNixFile" VARCHAR NOT NULL , "repo_timeout" INTEGER NOT NULL , "repo_cache__binaryCache_id" INTEGER, "repo_hookId" INTEGER NOT NULL , PRIMARY KEY("repo_id"));
INSERT INTO "ciDb_repos" SELECT "repo_id", "repo_accessAccount__connectedAccount_id", "repo_name", "repo_namespace", "repo_buildNixFile", "repo_timeout", NULL, "repo_hookId" FROM "ciDb_repos_old_1";
DROP TABLE ciDb_repos_old_1;
```

#### Removed s3cache column
```
ALTER TABLE "ciDb_ciSettings" RENAME TO "ciDb_ciSettings_old_0";
CREATE TABLE IF NOT EXISTS "ciDb_ciSettings"("ciSettings_id" INTEGER NOT NULL , "ciSettings_nixPath" VARCHAR NOT NULL , "ciSettings_serveLocalCache" BOOLEAN NOT NULL , PRIMARY KEY("ciSettings_id"));
INSERT INTO "ciDb_ciSettings" SELECT "ciSettings_id", "ciSettings_nixPath", "ciSettings_serveLocalCache" FROM "ciDb_ciSettings_old_0";
DROP TABLE ciDb_ciSettings_old_0;
```

#### Removed CachedHash autoincrementing ID

```
ALTER TABLE "ciDb_cachedHashes" RENAME TO "ciDb_cachedHashes_old_0";
CREATE TABLE IF NOT EXISTS "ciDb_cachedHashes"("cachedHash_hash" VARCHAR NOT NULL , "cachedHash_cache__binaryCache_id" INTEGER NOT NULL , "cachedHash_time" TIMESTAMP WITH TIME ZONE NOT NULL , PRIMARY KEY("cachedHash_hash"));
INSERT INTO "ciDb_cachedHashes" SELECT distinct("cachedHash_hash"), max("cachedHash_cache__binaryCache_id"), max("cachedHash_time") FROM "ciDb_cachedHashes_old_0" GROUP BY "cachedHash_hash";
DROP TABLE "ciDb_cachedHashes_old_0";
```
