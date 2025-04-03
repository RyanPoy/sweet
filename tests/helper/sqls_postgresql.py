CREATE_SQLS = [
    # create table foos for pg driver testing
    """CREATE TABLE IF NOT EXISTS "foos" (
        id SERIAL PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        code INT NOT NULL
    )""",

    # create table for old column testing
    """create table if not exists table_types (
        column_integer integer default 10 not null,
        column_bigint bigint,
        column_smallint smallint,
        column_varchar varchar,
        column_char char,
        column_text text,
        column_boolean boolean,
        column_real real,
        column_double double precision,
        column_decimal decimal,
        column_numeric numeric,
        column_date date,
        column_timestamp timestamp,
        column_timestamptz timestamptz,
        column_time time,
        column_bytea bytea
    )""",

    # create table users for the User model class
    """CREATE TABLE IF NOT EXISTS users (
        id SERIAL primary key,
        name varchar(64) not null
    )
    """
]

DROP_SQLS = [
    """drop table if exists table_types""",
    """DROP TABLE IF EXISTS foos""",
    """DROP TABLE IF EXISTS users""",
]
