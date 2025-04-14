import pathlib

DATABASE = {
    "driver": "sqlite",
    # "db"    : ':memory:',
    "db"     : str(pathlib.PurePath(__file__).parent.parent.joinpath("sweet.sqlite3")),
    'memory' : True
}
