import pathlib

db_path = pathlib.PurePath(__file__).parent.parent
DATABASE = {
    "drivers": "sqlite",
    # "db"    : ':memory:',
    "db"    : str(db_path.joinpath("sweet.sqlite3")),
    'memory': True
}
