from typing import Dict, List

from sweet.database.driver.base_driver import BaseDriver


class MySQLDriver(BaseDriver):

    async def columns(self, table_name: str) -> List[Dict]:
        sql = f"SHOW COLUMNS FROM `{table_name}`"
        rows = await self.fetchall(sql)
        # Field | Type | Null | Key | Default | Extra |
        # names = ('name', 'kind', 'null', 'key', 'default', 'extra')
        return [
            { 'name': r[0], 'kind': r[1], 'null': r[2], 'key': r[3], 'default': r[4], 'extra': r[5] } for r in rows
        ]


