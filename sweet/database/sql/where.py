class Where:

    def __init__(self, **kwargs):
        self.conditions = kwargs

    def _parse(self):
        if '__' in key:
            field, op = key.split('__', 1)
            sql_op = OPERATOR_MAP.get(op, '=')
            if op == 'in' and isinstance(value, list):
                value = f"({', '.join(map(str, value))})"
            elif op == 'isnull':
                value = '' if value else 'NOT NULL'
            else:
                value = f"'{value}'"
            return f"{field} {sql_op} {value}"
        else:
            return f"{key} = '{value}'"
