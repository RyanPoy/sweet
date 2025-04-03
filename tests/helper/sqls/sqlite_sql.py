CREATE_SQLS = [
    # create table foos for sqlite driver testings
    """CREATE TABLE IF NOT EXISTS \"foos\" (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name VARCHAR(255) NOT NULL,
        code INTEGER NOT NULL
    )""",

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
        id int auto_increment primary key,
        name varchar(64) not null
    )
    """
]

DROP_SQLS = [
    """drop table if exists table_types""",
    """DROP TABLE IF EXISTS foos""",
    """DROP TABLE IF EXISTS users""",
]
