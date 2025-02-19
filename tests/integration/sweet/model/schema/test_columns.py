import unittest

from sweet.model.schema.columns import Column, ColumnType, Columns
from tests.integration.sweet import helper


class TestColumns(unittest.IsolatedAsyncioTestCase):

    async def asyncSetUp(self):
        self.driver = await helper.init_mysql()
        await self.driver.execute("""
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
                column_float float , 
                column_double double , 
                column_decimal decimal , 
                column_numeric numeric , 
                column_date date , 
                column_datetime datetime , 
                column_timestamp timestamp , 
                column_time time , 
                column_blob blob , 
                column_varbinary varbinary(1024)
            )"""
        )

    async def asyncTearDown(self):
        await self.driver.execute("drop table if exists table_types")
        await helper.close(self.driver)

    async def test_columns(self):
        """测试插入和查询功能."""
        expected = Columns()
        expected.append(Column(name="column_int", kind=ColumnType.Integer, limit=0, null="YES", default=None, key="", extra="", precision=0, scale=0))
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
        expected.append(Column(name="column_float", kind="float", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_double", kind="double", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_decimal", kind="decimal", limit=0, null=True, default=None, key="", extra="", precision=10, scale=0))
        expected.append(Column(name="column_numeric", kind="numeric", limit=0, null=True, default=None, key="", extra="", precision=10, scale=0))
        expected.append(Column(name="column_date", kind="date", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_datetime", kind="datetime", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_timestamp", kind="timestamp", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_time", kind="time", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_blob", kind="blob", limit=0, null=True, default=None, key="", extra="", precision=0, scale=0))
        expected.append(Column(name="column_varbinary", kind="varbinary", limit=1024, null=True, default=None, key="", extra="", precision=0, scale=0))

        cs = await Columns.of(self.driver, 'table_types')
        for i, e in enumerate(expected.data):
            self.assertEqual(str(e), str(list(cs.data)[i]))
        self.assertEqual(expected, cs)


if __name__ == "__main__":
    unittest.main()

