class Query:
    _alias_counter = 0  # 用于生成唯一别名的计数器

    def __init__(self):
        self.alias = None  # 当前 Query 的别名
        self.subquery = None  # 嵌套的子查询
        self.parent = None  # 上层 Query，用于检测循环嵌套

    @classmethod
    def _generate_alias(cls):
        """生成唯一别名"""
        cls._alias_counter += 1
        return f"subquery_{cls._alias_counter}"

    def from_(self, subquery):
        """
        定义子查询，并生成别名。
        确保无循环嵌套，并为每个查询分配唯一的别名。
        """
        if not isinstance(subquery, Query):
            raise ValueError("Subquery must be an instance of Query.")

        # 检查循环嵌套
        if self._has_circular_reference(subquery):
            raise ValueError("Circular reference detected in subquery nesting.")

        # 设定子查询
        self.subquery = subquery
        subquery.parent = self  # 设置父查询
        subquery.alias = subquery.alias or self._generate_alias()  # 生成或保留唯一别名
        return self

    def _has_circular_reference(self, subquery):
        """
        检测循环嵌套：通过追踪父级链，检查是否有重复引用。
        """
        current = self
        while current:
            if current is subquery:
                return True
            current = current.parent
        return False

    def __repr__(self):
        """返回当前 Query 的 SQL 结构表示（仅供调试）"""
        subquery_repr = repr(self.subquery) if self.subquery else "None"
        return f"Query(alias={self.alias}, subquery={subquery_repr})"


# 示例用法
q1 = Query()
q2 = Query()
q3 = Query()

# 嵌套 from_ 调用
q1.from_(q2)
q2.from_(q3)

# 检测自动生成的别名
print(q1)  # Query(alias=None, subquery=Query(alias=subquery_1, ...))
print(q2)  # Query(alias=subquery_1, subquery=Query(alias=subquery_2, ...))
print(q3)  # Query(alias=subquery_2, subquery=None)

# 防止循环嵌套
try:
    q3.from_(q1)  # 循环嵌套
except ValueError as e:
    print(f"Error: {e}")

