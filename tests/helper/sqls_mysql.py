CREATE_SQLS = [
    # create table for auto column testing
    """create table if not exists table_types (
        column_int int , 
        column_tinyint tinyint , 
        column_smallint smallint , 
        column_mediumint mediumint , 
        column_bigint bigint , 
        column_varchar varchar(32) , 
        column_char char , 
        column_text text , 
        column_longtext longtext , 
        column_mediumtext mediumtext , 
        column_boolean boolean , 
        column_float float not null default 3.5, 
        column_double double , 
        column_decimal decimal , 
        column_numeric numeric , 
        column_date date, 
        column_datetime datetime , 
        column_timestamp timestamp , 
        column_time time , 
        column_blob blob , 
        column_varbinary varbinary(1024)
    )""",

    # create table users for the User model class
    """CREATE TABLE IF NOT EXISTS users (
        id INT PRIMARY KEY AUTO_INCREMENT,
        name varchar(64) not null
    )
    """,
]

DROP_SQLS = [
    """drop table if exists table_types""",
    """DROP TABLE IF EXISTS users""",
]
