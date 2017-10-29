# Chaordic Schema
The overarching goal of _Chaordic Schema_ is to take the drudgery out of defining encoders, decoders, persistence code and Scalacheck generators,
while losing no typesafety.

A secondary goal is to provide a uniform mechanism for validating types across system boundaries with Cats `Validated`, for instance when parsing a type from JSON, or when reading it from a database.

This library makes extensive use of [Shapeless](https://github.com/milessabin/shapeless), [Cats](https://github.com/typelevel/cats) and [Enumeratum](https://github.com/lloydmeta/enumeratum) to achieve its goals, and will shortly provide the following bindings:

* [Circe](https://github.com/circe/circe) for automatic derivation of JSON `Encoders` and `Decoders`.
* [Argonaut](http://argonaut.io/) as an alternative to Circe.
* [Scalacheck](https://www.scalacheck.org/) for automatic derivation of Scalacheck `Gen` for arbitrary case classes.
* [Http4s](http://http4s.org/) for parsing incoming HTTP requests, where JSON is not used.
* **SQL/JDBC** - to easily persist/read arbitrary case classes from a database. Will initially be tested against Postgresql.

These bindings will be on a modular _"pick and choose "_-basis, with separate (but compatible) libraries for each binding.

### Pre-requisites
You need PostgreSQL installed for the _jdbc_ sub project, with the following setup:

    createdb schema_test
    createuser schema_test
    psql postgres
    alter user schema_test with encrypted password 'password';
    grant all privileges on database schema_test to schema_test;