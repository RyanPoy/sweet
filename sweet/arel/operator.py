from sweet.utils import BasicType

OPERATOR_DELIMITER = '__'
OPERATOR_MAP = {
    'lt': '<',
    'lte': '<=',

    'gt': '>',
    'gte': '>=',

    'not': '<>',
    'like': 'LIKE',
    'not_like': "NOT LIKE",
}


def opt(key: str, value: BasicType, placeholder: str) -> (str, list):
    if OPERATOR_DELIMITER not in key:
        if value is None:
            return f"{key} IS NULL", None
        if isinstance(value, (tuple, list)):
            return f"{key} IN ({', '.join([placeholder]*len(value))})", value
        return f'{key} = {placeholder}', [value]

    vs = key.split(OPERATOR_DELIMITER, 1)
    col, op_type = vs[0], vs[1]
    if op_type == 'not':
        if value is None:
            return f"{col} IS NOT NULL", None
        if isinstance(value, (tuple, list)):
            return f"{col} NOT IN ({', '.join([placeholder]*len(value))})", value
        return f"{col} <> {placeholder}", [value]
    elif op_type == 'bt' or op_type == 'not_bt':
        if not isinstance(value, (tuple, list)) or not len(value) == 2:
            raise TypeError('The bt or not_bt operation expects a list or tuple of length 2, but it is not.')
        between_op = 'BETWEEN' if op_type == 'bt' else 'NOT BETWEEN'
        return f"{col} {between_op} {placeholder} AND {placeholder}", value

    return f"{col} {OPERATOR_MAP.get(op_type, '=')} {placeholder}", value if isinstance(value, (tuple, list)) else [value]


