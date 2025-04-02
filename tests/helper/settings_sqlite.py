import pathlib

db_path = pathlib.PurePath(__file__).parent.parent
DATABASE = {
    "driver": "sqlite",
    # "db"    : ':memory:',
    "db"    : str(db_path.joinpath("sweet.sqlite3")),
    'memory': True
}
