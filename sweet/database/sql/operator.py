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


def opt(key, value, placeholder):
    if OPERATOR_DELIMITER not in key:
        if value is None:
            return f"{key} IS NULL"
        if isinstance(value, (tuple, list)):
            return f"{key} IN ({', '.join([placeholder]*len(value))})"
        return f'{key} = {placeholder}'

    vs = key.split(OPERATOR_DELIMITER, 1)
    col, op_type = vs[0], vs[1]
    if op_type == 'not':
        if value is None:
            return f"{col} IS NOT NULL"
        if isinstance(value, (tuple, list)):
            return f"{col} NOT IN ({', '.join([placeholder]*len(value))})"
        return f"{col} <> {placeholder}"
    elif op_type == 'bt' or op_type == 'not_bt':
        if not isinstance(value, (tuple, list)) or not len(value) == 2:
            raise TypeError('The bt or not_bt operation expects a list or tuple of length 2, but it is not.')
        between_op = 'BETWEEN' if op_type == 'bt' else 'NOT BETWEEN'
        return f"{col} {between_op} {placeholder} AND {placeholder}"

    return f"{col} {OPERATOR_MAP.get(op_type, '=')} {placeholder}"


