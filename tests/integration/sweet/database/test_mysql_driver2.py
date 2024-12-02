import asyncio

import pytest
from sweet.database.driver import *

@pytest.fixture(scope="module")
async def driver():
    """Initialize MySQL Driver."""
    driver = Driver(**{
        "host": "127.0.0.1",
        "port": 3306,
        "user": "root",
        "password": "",
        "db": "sweet",
    })
    await driver.init_pool()
    try:
        yield driver
    finally:
        # await asyncio.wait_for(driver.close_pool(), 10)
        await driver.close_pool()

@pytest.fixture(scope="module")
async def setup_database(driver):
    """在测试前创建表并在测试后清理表."""
    try:
        await driver.execute("""
            CREATE TABLE IF NOT EXISTS foos (
                id INT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(255) NOT NULL,
                code INT NOT NULL
            );"""
        )
        yield
    finally:
        await driver.execute("""DROP TABLE IF EXISTS foos;""")

@pytest.mark.asyncio
async def test_select_without_transaction(driver, setup_database):
    """测试非事务情况下的查询."""
    select_query = "SELECT COUNT(*) as count FROM foos"
    result = await driver.fetchone(select_query)
    assert result == (0,)
