from sweet.sequel.nodes.sql_literal import SqlLiteral
from sweet.sequel.visitor.postgresql import PostgreSQL
from sweet.sequel.visitor.sqlite import SQLite
from sweet.sequel.visitor.mysql import MySQL

mysql = MySQL()
sqlite = SQLite()
pg = PostgreSQL()


def sql(s) -> SqlLiteral:
    return SqlLiteral(s)

# 在 Rails 7 的 Arel 中，Node 类是所有节点类的基础，它提供了一些通用方法，支持构建和操作 SQL 查询。这些方法包括：
#
# 	1.	逻辑操作方法：
# 	•	#and、#or、#not 等方法，用于在 SQL 查询中组合逻辑表达式。
# 	2.	相等性检查：
# 	•	#equality? 方法，用于检查两个节点是否等价，常用于语法树的比较。
# 	3.	表达式反转：
# 	•	#invert 方法，用于生成反转表达式（例如，将 = 1 转为 != 1）。
# 	4.	SQL 转换：
# 	•	#to_sql 方法，可以将节点转换为 SQL 字符串，这是 Arel 最核心的功能之一，用于最终生成 SQL 代码。
# 	5.	属性访问：
# 	•	#fetch_attribute 方法，帮助节点与特定的属性交互，例如获取字段或表的元数据。
#
# 这些通用方法的存在，使得 Arel 的节点具有高度的一致性和扩展性。像 Unary 和 Binary 这样的子类，继承了这些基本行为，同时扩展了各自特有的功能。例如，Unary 表示仅有一个子节点的操作（如 NOT），而 Binary 表示有两个子节点的操作（如加法或比较）。通过 Node 的统一接口，Arel 能够处理所有节点类型，简化了 SQL 构建的逻辑。
#
# 这种设计体现了 Rails 中常见的面向对象编程原则，即通过继承实现代码复用和多态性。即便子类没有显式使用父类的构造器逻辑，它们仍然可以通过方法继承享受到 Node 提供的核心功能 [oai_citation:2,Class: Arel::Nodes::Grouping
