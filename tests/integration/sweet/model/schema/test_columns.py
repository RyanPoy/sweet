import unittest

from sweet.model.schema._column import Column, ColumnKind, Columns
from tests.integration import integration_helper


class TestColumns(unittest.IsolatedAsyncioTestCase):

    async def asyncSetUp(self):
        self.mysql_driver = await integration_helper.init_mysql()
        await self.mysql_driver.execute("""
            create table if not exists table_types (
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
            )"""
        )

        self.sqlite_driver = await integration_helper.init_sqlite()
        await self.sqlite_driver.execute("""
            create table if not exists table_types (
                column_integer integer not null default 30,
                column_text text,
                column_real real,
                column_date date,
                column_datetime datetime,
                column_time time,
                column_blob blob
            )"""
        )

        self.pg_driver = await integration_helper.init_postgres()
        await self.pg_driver.execute("""
            create table if not exists table_types (
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
            );"""
        )

    async def asyncTearDown(self):
        await self.mysql_driver.execute("drop table if exists table_types")
        await self.sqlite_driver.execute("drop table if exists table_types")
        await self.pg_driver.execute("drop table if exists table_types")

        await integration_helper.close(self.mysql_driver)
        await integration_helper.close(self.sqlite_driver)
        await integration_helper.close(self.pg_driver)

    async def test_mysql_columns(self):
        """测试插入和查询功能."""
        expected = Columns()
        expected.append(Column(name="column_int", kind=ColumnKind.Integer, limit=0, null="YES", default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_tinyint", kind="tinyint", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_smallint", kind="smallint", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_mediumint", kind="mediumint", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_bigint", kind="bigint", limit=0, null="YES", default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_varchar", kind="varchar", limit=32, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_char", kind="char", limit=1, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_text", kind="text", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_longtext", kind="longtext", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_mediumtext", kind="mediumtext", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_boolean", kind="tinyint", limit=1, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_float", kind="float", limit=0, null=False, default='3.5', key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_double", kind="double", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_decimal", kind="decimal", limit=0, null=True, default=None, key="", extra="", precision=10, scale=0))
        expected.append(Column(name="column_numeric", kind="numeric", limit=0, null=True, default=None, key="", extra="", precision=10, scale=0))
        expected.append(Column(name="column_date", kind="date", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_datetime", kind="datetime", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_timestamp", kind="timestamp", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_time", kind="time", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_blob", kind="blob", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_varbinary", kind="varbinary", limit=1024, null=True, default=None, key="", extra="", precision=0, scale=0))

        cs = await Columns.of(self.mysql_driver, 'table_types')
        for i, e in enumerate(expected.data):
            self.assertEqual(str(e), str(list(cs.data)[i]))
        self.assertEqual(expected, cs)

    async def test_sqlite_columns(self):
        expected = Columns()
        expected.append(Column(name="column_integer", kind="integer", limit=0, null=False, default="30", key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_text", kind="text", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_real", kind="real", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_date", kind="date", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_datetime", kind="datetime", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_time", kind="time", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
        expected.append(Column(name="column_blob", kind="blob", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))

        cs = await Columns.of(self.sqlite_driver, 'table_types')
        for i, e in enumerate(expected.data):
            self.assertEqual(str(e), str(list(cs.data)[i]))
        self.assertEqual(expected, cs)

    async def test_pg_columns(self):
        expected = Columns()
        expected.append(Column(name="column_integer", kind="integer", limit=0, null=False, default='10', key="", extra=""))
        expected.append(Column(name="column_bigint", kind="bigint", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_smallint", kind="smallint", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_varchar", kind="varchar", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_char", kind="char", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_text", kind="text", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_boolean", kind="boolean", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_real", kind="real", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_double", kind="double precision", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_decimal", kind="decimal", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_numeric", kind="numeric", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_date", kind="date", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_timestamp", kind="timestamp", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_timestamptz", kind="timestamptz", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_time", kind="time", limit=0, null=True, default=None, key="", extra=""))
        expected.append(Column(name="column_bytea", kind="byte", limit=0, null=True, default=None, key="", extra=""))

        cs = await Columns.of(self.pg_driver, 'table_types')
        for i, e in enumerate(expected.data):
            c = cs.find_by_name(e.name)
            self.assertEqual(str(e), str(c))

if __name__ == "__main__":
    unittest.main()

