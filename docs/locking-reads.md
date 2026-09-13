# Locking reads (`SELECT ... FOR UPDATE` and friends)

This document records the research behind the locking API in
`Database.Beam.MySQL.Full`: what the SQL standard says, what MySQL and MariaDB
accept, and how they differ.

Last verified: 2026-09-13.

## Summary

- The SQL standard does not define row-locking clauses for `SELECT`.
  `FOR SHARE`, `NOWAIT`, `SKIP LOCKED` and `WAIT n` are all vendor extensions.
- MySQL and MariaDB accept different syntax. In particular, **there is no
  syntax for a shared lock with `NOWAIT` / `SKIP LOCKED` that both servers
  accept**: MySQL only accepts `FOR SHARE NOWAIT`, MariaDB only accepts
  `LOCK IN SHARE MODE NOWAIT`.
- Therefore beam-mysql-haskell renders the locking clause exactly as specified.
  It neither detects the server flavor nor rewrites one form into another, so
  the generated SQL is always predictable. Choosing a form that the target
  server accepts is up to the user.
- `OF tbl_name` and multiple locking clauses (MySQL 8.0.1+ only) are not
  supported yet.

## SQL standard

The SQL:2016 Foundation grammar has only the *updatability clause* of a cursor
specification:

```
<cursor specification> ::= <query expression> [ <updatability clause> ]
<updatability clause>  ::= FOR { READ ONLY | UPDATE [ OF <column name list> ] }
```

- It is part of a cursor specification (`DECLARE CURSOR`), not of an ordinary
  `SELECT` statement.
- `OF` takes a list of *columns*, not tables.
- `SHARE`, `NOWAIT`, `SKIP LOCKED` and `WAIT` do not appear in the grammar.

The PostgreSQL manual describes the same situation: `FOR UPDATE` is allowed by
the standard only in `DECLARE CURSOR`, and `FOR SHARE`, `NOWAIT` and
`SKIP LOCKED` do not appear in the standard.

## MySQL

### MySQL 5.7

```
opt_select_lock_type:
    /* empty */
  | FOR UPDATE
  | LOCK IN SHARE MODE
```

No `FOR SHARE`, `NOWAIT`, `SKIP LOCKED` or `OF`.

### MySQL 8.0.1 and later (8.0, 8.4 LTS, 9.x, trunk)

```
locking_clause_list:
    locking_clause_list locking_clause
  | locking_clause

locking_clause:
    FOR lock_strength [locked_row_action]
  | FOR lock_strength OF table_alias_ref_list [locked_row_action]
  | LOCK IN SHARE MODE

lock_strength:     UPDATE | SHARE
locked_row_action: SKIP LOCKED | NOWAIT
```

(Simplified from `sql/sql_yacc.yy`.)

- `FOR SHARE`, `NOWAIT`, `SKIP LOCKED` and `OF` were introduced in 8.0.1.
- `LOCK IN SHARE MODE` is equivalent to `FOR SHARE` and remains for backward
  compatibility. It accepts no options. The trunk grammar does not emit a
  deprecation warning for it.
- There is no `WAIT n`. The wait time is controlled by
  `innodb_lock_wait_timeout`.
- Multiple locking clauses are allowed, but a clause without `OF` cannot be
  combined with another clause, and the same table cannot appear in more than
  one clause.
- If a table has an alias, `OF` must use the alias. Otherwise it must use the
  table name.
- `NOWAIT` and `SKIP LOCKED` apply only to row-level locks.
- A `NOWAIT` locking read that cannot acquire a lock fails with error 3572
  (`ER_LOCK_NOWAIT`) instead of 1205 (`ER_LOCK_WAIT_TIMEOUT`).

## MariaDB

```
select_lock_type:
    FOR UPDATE [lock_option]
  | LOCK IN SHARE MODE [lock_option]

lock_option:
    WAIT n | NOWAIT | SKIP LOCKED
```

(Simplified from `sql/sql_yacc.yy` of the `main` branch, version 13.2.)

- `WAIT n` and `NOWAIT` were introduced in 10.3.0 (MDEV-11379).
- `SKIP LOCKED` was introduced in 10.6.0 (MDEV-13115). It cannot be combined
  with `WAIT` / `NOWAIT` (MDEV-25433).
- Only one option can be specified.
- `FOR SHARE` is not supported in any version, including the current `main`
  branch.
- `OF tbl_name` is not supported (MDEV-17514, still open).
- Only one locking clause can be specified.
- `WAIT n` / `NOWAIT` set the lock wait timeout for the statement. `WAIT 0` is
  equivalent to `NOWAIT`. The same options also exist on DDL and
  `LOCK TABLES`.
- A locking read that times out, including with `NOWAIT`, fails with error
  1205 (`ER_LOCK_WAIT_TIMEOUT`).

## Compatibility matrix

| Rendered SQL | MySQL 5.7 | MySQL 8.0.1+ | MariaDB 10.3–10.5 | MariaDB 10.6+ |
|---|:-:|:-:|:-:|:-:|
| `FOR UPDATE` | ✓ | ✓ | ✓ | ✓ |
| `LOCK IN SHARE MODE` | ✓ | ✓ | ✓ | ✓ |
| `FOR SHARE` | ✗ | ✓ | ✗ | ✗ |
| `FOR UPDATE NOWAIT` | ✗ | ✓ | ✓ | ✓ |
| `FOR UPDATE SKIP LOCKED` | ✗ | ✓ | ✗ | ✓ |
| `FOR UPDATE WAIT n` | ✗ | ✗ | ✓ | ✓ |
| `FOR SHARE NOWAIT` / `FOR SHARE SKIP LOCKED` | ✗ | ✓ | ✗ | ✗ |
| `FOR SHARE WAIT n` | ✗ | ✗ | ✗ | ✗ |
| `LOCK IN SHARE MODE NOWAIT` | ✗ | ✗ | ✓ | ✓ |
| `LOCK IN SHARE MODE SKIP LOCKED` | ✗ | ✗ | ✗ | ✓ |
| `LOCK IN SHARE MODE WAIT n` | ✗ | ✗ | ✓ | ✓ |
| `OF tbl_name` / multiple locking clauses | ✗ | ✓ | ✗ | ✗ |

Summary for currently supported releases:

- Portable: `FOR UPDATE`, `FOR UPDATE NOWAIT`, `FOR UPDATE SKIP LOCKED`, and
  `LOCK IN SHARE MODE` without options.
- MySQL only: `FOR SHARE` (with or without options), `OF`.
- MariaDB only: `WAIT n`, `LOCK IN SHARE MODE` with options.

### Checked on a real server

The MariaDB columns were also checked against MariaDB 11.8.6. All forms marked
✓ above were accepted. `FOR SHARE`, `FOR SHARE NOWAIT`,
`FOR SHARE SKIP LOCKED`, `FOR UPDATE WAIT 1 SKIP LOCKED`,
`FOR UPDATE NOWAIT SKIP LOCKED` and `FOR UPDATE OF lockcheck` were rejected
with error 1064 (syntax error). While another transaction held a row lock,
`NOWAIT` and `WAIT 1` failed with error 1205, and `SKIP LOCKED` returned only
the unlocked rows.

The MySQL columns are based on the grammar and the reference manual. They
were not checked on a real server.

## Common notes

- Locking reads take effect only inside a transaction (`autocommit=0` or
  `START TRANSACTION`).
- A locking clause on the outer query does not lock rows read by a subquery
  unless the subquery has its own locking clause (MySQL manual).
- `SKIP LOCKED` returns an inconsistent view of the data. It is meant for
  queue-like tables, not for general transactional work.
- Statements using `SKIP LOCKED` are unsafe for statement-based replication on
  both servers. MySQL also treats `NOWAIT` as unsafe.

## Server lifecycle (as of 2026-09-13)

| Release | End of life |
|---|---|
| MySQL 5.7 | 2023-10-31 |
| MySQL 8.0 | 2026-04-30 |
| MySQL 8.4 LTS | 2032-04-30 |
| MySQL 9.7 LTS | 2034-04-21 |
| MariaDB 10.3 | 2023-05-25 |
| MariaDB 10.5 | 2025-06-24 |
| MariaDB 10.6 LTS | 2026-07-06 |
| MariaDB 10.11 LTS | 2028-02-16 |
| MariaDB 11.4 LTS | 2029-05-29 |
| MariaDB 11.8 LTS | 2028-06-04 |
| MariaDB 12.3 LTS | 2029-06-12 |

Every supported MySQL release accepts `FOR SHARE`, `NOWAIT`, `SKIP LOCKED`
and `OF`. Every supported MariaDB release accepts `WAIT n`, `NOWAIT` and
`SKIP LOCKED`.

## Sources

SQL standard

- [SQL:2016 Foundation grammar (Jake Wheat's sql-overview)](https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html), section 14.3 `<cursor specification>`
- [PostgreSQL: SELECT, Compatibility](https://www.postgresql.org/docs/current/sql-select.html#SQL-SELECT-COMPATIBILITY)

MySQL

- [MySQL 8.0 Reference Manual: SELECT Statement](https://dev.mysql.com/doc/refman/8.0/en/select.html)
- [MySQL 8.4 Reference Manual: SELECT Statement](https://dev.mysql.com/doc/refman/8.4/en/select.html)
- [MySQL 8.0 Reference Manual: Locking Reads](https://dev.mysql.com/doc/refman/8.0/en/innodb-locking-reads.html)
- [MySQL 8.0.1: Using SKIP LOCKED and NOWAIT to handle hot rows](https://dev.mysql.com/blog-archive/mysql-8-0-1-using-skip-locked-and-nowait-to-handle-hot-rows/)
- [Changes in MySQL 8.0.1](https://dev.mysql.com/doc/relnotes/mysql/8.0/en/news-8-0-1.html)
- `sql/sql_yacc.yy` of mysql/mysql-server:
  - [5.7 @ f7680e98](https://github.com/mysql/mysql-server/blob/f7680e98b6bbe3500399fbad465d08a6b75d7a5c/sql/sql_yacc.yy#L9237) (`opt_select_lock_type`)
  - [8.0 @ 9fc8d600](https://github.com/mysql/mysql-server/blob/9fc8d60003b4eee32767c1e30942f222c3508c31/sql/sql_yacc.yy#L10129) (`locking_clause`)
  - [trunk @ fcf22d38](https://github.com/mysql/mysql-server/blob/fcf22d38b9097f2edaf450c76957218c5f4af509/sql/sql_yacc.yy#L10350) (`locking_clause`)
- [`share/messages_to_clients.txt` of mysql/mysql-server trunk @ fcf22d38](https://github.com/mysql/mysql-server/blob/fcf22d38b9097f2edaf450c76957218c5f4af509/share/messages_to_clients.txt#L8153) (`ER_LOCK_NOWAIT`)

MariaDB

- [MariaDB: SELECT](https://mariadb.com/docs/server/reference/sql-statements/data-manipulation/selecting-data/select)
- [MariaDB: FOR UPDATE](https://mariadb.com/docs/server/reference/sql-statements/data-manipulation/selecting-data/for-update)
- [MariaDB: LOCK IN SHARE MODE](https://mariadb.com/docs/server/reference/sql-statements/data-manipulation/selecting-data/lock-in-share-mode)
- [MariaDB: WAIT and NOWAIT](https://mariadb.com/docs/server/reference/sql-statements/transactions/wait-and-nowait)
- [MariaDB 10.3.0 Release Notes](https://mariadb.com/docs/release-notes/community-server/old-releases/10.3/10.3.0)
- [MDEV-11379: SELECT FOR UPDATE WAIT](https://jira.mariadb.org/browse/MDEV-11379) (fixed in 10.3.0)
- [MDEV-13115: Implement SELECT [FOR UPDATE|LOCK IN SHARED MODE] SKIP LOCKED](https://jira.mariadb.org/browse/MDEV-13115) (fixed in 10.6.0)
- [MDEV-25433: SKIP LOCKED should imply NOWAIT](https://jira.mariadb.org/browse/MDEV-25433) (fixed in 10.6.0)
- [MDEV-17514: Please implement FOR UPDATE OF tablename,... and FOR SHARE OF tablename,...](https://jira.mariadb.org/browse/MDEV-17514) (open)
- [Incompatibilities and Feature Differences Between MariaDB 10.7 and MySQL 8.0](https://mariadb.com/docs/release-notes/community-server/about/compatibility-and-differences/incompatibilities-and-feature-differences-between-mariadb-and-mysql-unmaint/incompatibilities-and-feature-differences-between-mariadb-10-7-and-mysql-8)
- `sql/sql_yacc.yy` of MariaDB/server:
  [main @ 6f18dc2d](https://github.com/MariaDB/server/blob/6f18dc2d117a799998a75a6b39e4fb395460388c/sql/sql_yacc.yy#L9681) (`select_lock_type`, `opt_lock_wait_timeout_new`)

Others

- [MySQL vs. MariaDB: WAIT, NOWAIT, SKIP LOCKED (blog, 2018)](https://falseisnotnull.wordpress.com/2018/04/23/mysql-vs-mariadb-wait-nowait-skip-locked/)
- [beam-postgres: Database.Beam.Postgres.Full](https://hackage.haskell.org/package/beam-postgres/docs/Database-Beam-Postgres-Full.html) (API shape of `lockingFor_`)
- [endoflife.date: MySQL](https://endoflife.date/mysql), [endoflife.date: MariaDB](https://endoflife.date/mariadb)
