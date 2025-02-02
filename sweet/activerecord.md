# @TODO: 对 quoting.py 要进行重构

# User.where(active: true).limit(10)具体Relation是怎么调用 Arel的?

### 流程概述

1). 调用链开始：
   - 当开发者调用 User.where(active: true).limit(10) 时，它会触发 ActiveRecord 的 Relation 构建器。
2). 构建 Relation 对象：
   - where 和 limit 方法会将条件存储在 Relation 对象的内部。
3). 使用 Arel 构建 SQL：
   - 当调用 .to_sql 或执行查询（如 .to_a、.first 等）时，Relation 会调用 Arel 来构建最终的 SQL 字符串。
4). 执行 SQL：
   - 构建的 SQL 被传递到数据库适配器，执行后返回结果。


### 核心代码流程

1).	调用 User.where(active: true)
   - User 是一个 ActiveRecord 模型，它继承自 ActiveRecord::Base。
   - where 方法会返回一个 Relation 对象，包含查询条件。    
```ruby
# lib/active_record/relation/query_methods.rb
def where(opts = :chain, *rest)
    spawn.where!(opts, *rest)
end
```

   - spawn 方法克隆当前 Relation 对象，保证链式调用不会修改原始 Relation。
   - 查询条件会存储在 Relation 的内部 where_values 中。

2).	调用 limit(10)
   - limit 方法也会返回一个 Relation 对象，存储 limit 条件。
```ruby
# lib/active_record/relation/query_methods.rb
def limit(value)
  spawn.limit!(value)
end
```
   - limit! 方法将值存储在 Relation 的 limit_value 中。

3).	执行查询
   - 当调用 .to_a 或 .to_sql 时，Relation 开始调用 Arel 来构建 SQL。

4).	Arel 的调用
   - Relation 的 build_arel 方法负责将内部条件转换为 Arel 的节点（AST）。
```ruby
# lib/active_record/relation.rb
def build_arel
  arel = table.arel

  build_where(arel, @where_values)
  build_limit(arel, @limit_value)

  arel
end
```
   - table.arel 返回一个 Arel::Table 对象，用于表示当前模型的数据库表。

5).	构建 WHERE 子句
   - build_where 方法将 @where_values 转换为 Arel 节点：
```ruby
# lib/active_record/relation/query_methods.rb
def build_where(arel, conditions)
  conditions.each do |condition|
    arel.where(klass.predicate_builder.build(condition))
  end
end
```
   - predicate_builder 会将 active: true 转换为 Arel::Nodes::Equality 节点。

6).	构建 LIMIT 子句
   - build_limit 方法会调用 Arel 的 take 方法：
```ruby
# lib/active_record/relation/query_methods.rb
def build_limit(arel, limit)
  arel.take(limit) if limit
end
```

7).生成 SQL
   - 最终的 Arel 对象会通过 to_sql 方法生成 SQL 字符串：
```ruby
# lib/arel/tree_manager.rb
def to_sql
  @engine.connection.visitor.accept(ast)
end   
```

### 调用链图解
```text
User.where(active: true).limit(10)
   |
   +-- Relation#where -> Stores condition in @where_values
   |
   +-- Relation#limit -> Stores limit in @limit_value
   |
   +-- Relation#to_sql
         |
         +-- Relation#build_arel
               |
               +-- Arel::Table -> Represents the "users" table
               |
               +-- Arel::Nodes -> Constructs WHERE and LIMIT nodes
               |
               +-- Arel::TreeManager#to_sql
```

### 最终生成的 Arel 树

在这个例子中，生成的 Arel 树可能类似于：
```ruby
Arel::SelectManager.new(Arel::Table.new(:users))
  .project(Arel.star)
  .where(Arel::Nodes::Equality.new(users[:active], true))
  .take(10)
```

最终转换为 SQL：
```sql
SELECT "users".* FROM "users" WHERE "users"."active" = 1 LIMIT 10
```

### 总结

1). Relation 构建查询条件并存储在对象内部。

2). 在执行查询时，Relation 使用 Arel 将这些条件转换为 SQL 表达式。

3). Arel 提供灵活的 AST 构建能力，生成 SQL 的结构化表示，并最终转换为 SQL 字符串。

**通过这种设计，Rails 实现了查询逻辑的分层解耦，开发者操作的是 Relation（高层接口），而 Arel 专注于底层 SQL 生成。**

