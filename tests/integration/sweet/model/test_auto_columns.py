import pytest
from sweet.models.auto_column import Column, ColumnKind, Columns


@pytest.mark.asyncio
async def test__oldcolumns_mysql(mysql_env):
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

    cs = await Columns.of(mysql_env.db, 'table_types')
    for i, e in enumerate(expected.data):
        assert str(list(cs.data)[i]) == str(e)
    assert cs == expected


@pytest.mark.asyncio
async def test__oldcolumns_sqlite(sqlite_env):
    expected = Columns()
    expected.append(Column(name="column_integer", kind="integer", limit=0, null=False, default="30", key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_text", kind="text", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_real", kind="real", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_date", kind="date", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_datetime", kind="datetime", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_time", kind="time", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))
    expected.append(Column(name="column_blob", kind="blob", limit=0, null=True, default=None, key="", extra="0", precision=0, scale=0))

    cs = await Columns.of(sqlite_env.db, 'table_types')
    for i, e in enumerate(expected.data):
        assert str(list(cs.data)[i]) == str(e)
    assert cs, expected


@pytest.mark.asyncio
async def test_pg_columns(pg_env):
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

    cs = await Columns.of(pg_env.db, 'table_types')
    for i, e in enumerate(expected.data):
        c = cs.find_by_name(e.name)
        assert str(e) == str(c)
