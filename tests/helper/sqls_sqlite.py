CREATE_SQLS = [
    # create table for old column testing
    """create table if not exists table_types (
        column_integer integer not null default 30,
        column_text text,
        column_real real,
        column_date date,
        column_datetime datetime,
        column_time time,
        column_blob blob
    )""",

    # create table users for the User model class
    """CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY,
        name varchar(64) not null
    )
    """
]

DROP_SQLS = [
    """drop table if exists table_types""",
    """DROP TABLE IF EXISTS users""",
]
