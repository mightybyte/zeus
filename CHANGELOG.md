# Zeus Changelog

## 2020-06-27 Beam upgrade requires DB migration / reset

An upgrade to the Beam DB library resulted in columns with type INTEGER changing
to BIGINT. See [migrations.md](migrations.md) for more information.
