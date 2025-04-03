CREATE_SQLS = [
    # create table users
    """CREATE TABLE IF NOT EXISTS users (
        id SERIAL primary key,
        name varchar(64) not null
    )
    """
]

DROP_SQLS = [
    """DROP TABLE IF EXISTS users""",
]
