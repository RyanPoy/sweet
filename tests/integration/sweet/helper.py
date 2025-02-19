from sweet.database.driver import Driver, MySQLDriver


async def init_mysql() -> Driver:
    mysql = MySQLDriver(**{
        "host"    : "127.0.0.1",
        "port"    : 3306,
        "user"    : "root",
        "password": "",
        "db"      : "sweet",
    })
    await mysql.init_pool(1, 1)
    return mysql


async def close(driver: Driver) -> None:
    try:
        await driver.close_pool()
    except:
        pass
