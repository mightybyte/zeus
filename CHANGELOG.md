# Zeus Changelog

## 2020-07-15 Change DB keys to Int32

An upgrade to the Beam DB library resulted in columns with type INTEGER changing
to BIGINT. We avoid a migration by changing these types to Int32.
