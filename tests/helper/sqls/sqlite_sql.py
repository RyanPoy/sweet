CREATE_SQLS = [
    # create table foos for test_mysql_driver
    """CREATE TABLE IF NOT EXISTS \"foos\" (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name VARCHAR(255) NOT NULL,
        code INTEGER NOT NULL
    )""",

    # create table users for the User model class
    """CREATE TABLE IF NOT EXISTS users (
        id int auto_increment primary key,
        name varchar(64) not null
    )
    """
]

DROP_SQLS = [
    """DROP TABLE IF EXISTS foos""",
    """DROP TABLE IF EXISTS users""",
]
