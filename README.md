# sweet
=======
## list
- Introduction
- Retrieving Results
  - Chunking Results
  - Aggregates
- Selects
- Raw Expressions
- Joins
  - Inner Join Clause
  - Left Join Clause
  - Right Join Clause
  - Cross Join Clause
  - Advanced Join Clauses
- Unions
- Where Clauses
  - Parameter Grouping
  - Where Exists Clauses
  - JSON Where Clauses
- Ordering, Grouping, Limit, & Offset
- Conditional Clauses
- Inserts
- Updates
  - Updating JSON Columns
  - Increment & Decrement


## Introduction
python web framework looks like rails

## Retrieving Results
db.table('users').all()

```
SELECT * FROM `users`
```

db.table('users').first()

```
SELECT * FROM `users` limit 1
```

db.table('users').last()

```
SELECT * FROM `users`
```
> note: then get last record


### Chunking Results
> 待补充

### Aggregates
db.table('users').count()

```
SELECT COUNT(*) FROM `users`
```

db.table('orders').max('price')

```
SELECT MAX(`price`) FROM orders
```

> just support: count、max、min、avg、sum


## Selects
db.table('users').select('age').all()

```
SELECT `age` FROM `users`
```

db.table('users').select('age', 'name).all()

```
SELECT age, name FROM users
```

db.table('users').select('age').select('name').all()

```
SELECT age, name FROM users
```

## Raw Expressions
```
rs = db.raw('select * from users')
for r in rs:
  print rs
```
> Raw statements will be injected into the query as strings, so you should be extremely careful to not create SQL injection vulnerabilities.

## Joins
### Inner Join Clause
db.table('users').join('posts', users__id="posts.user_id").all()

``` 
SELECT * FROM `users` INNER JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```  

If you would like to perform a "left join" instead of an "inner join", use the leftJoin method. The leftJoin method has the same signature as the join method:
            
### Left Join Clause
db.table('users').left_join('posts', on="users.id = posts.user_id").all()

``` 
SELECT * FROM `users` LEFT JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Right Join Clause
db.table('users').right_join('posts', on="users.id = posts.user_id").all()

```
SELECT * FROM `users` RIGHT JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Cross Join Clause
db.table('users').cross_join('posts', on="users.id = posts.user_id").all()

```
SELECT * FROM `users` CROSS JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Advanced Join Clauses
```
def complex(join):
    join.on('user.id=contacts.user_id').and_(user__id=10).or_(user__name='abc')
db.table('users').join('contacts', complex).all()
```

```
SELECT * FROM `users` INNER JOIN `contacts` ON `user`.`id`=`contacts`.`user_id` AND `user`.`id` = 10 OR `user`.`name` = 'abc'
```
Note：If you would like to use a "where" style clause on your joins, you may use the "and_" and "or_" methods on a join. Instead of comparing two columns, these methods will compare the column against a value

## Unions
```
db.table('users').where(first_name=None).union(
	db.table('users').where(last_name=None)
).all()
```

```
SELECT * FROM `users` WHERE first_name IS NULL UNION SELECT * FROM `users` WHERE last_name IS NULL 
```

## Where Clauses
### Parameter Grouping
> 待补充

### Where Exists Clauses
> 待补充

### JSON Where Clauses
> 待补充

## Ordering, Grouping, Limit, & Offset
> 待补充

## Conditional Clauses
> 待补充

## Inserts
> 待补充

## Updates
### Updating JSON Columns
> 待补充

### Increment & Decrement
> 待补充


===





# ORM

- articles
  - id
  - title
  - content
  - category_id

- categories
  - id
  - name

- tags
  - id
  - name

- article_tags
  - id
  - article_id
  - tag_id


Artcile.all()   ==> Collection (element type is Article)
> SELECT * FROM `articles`


Article.offset(10).first() ==> Article
> SELECT * FROM `articles` LIMIT 1 OFFSET 10


Article.first().category  ==> Category
> a = Article.first() ==> SELECT * FROM `articles` LIMIT 1
> a.categroy  ==> SELECT * FROM `categories` WHERE `categories`.`id` = 2 LIMIT 1


Article.with_('category').first().category ==> Category
> SELECT * FROM `articles` LIMIT 1
> SELECT * FROM `categories` WHERE `categories`.`id` IN (0)


Category.first().articles  ==> Collection (element type is Article)
> c = Category.first() ==> SELECT * FROM `categories` LIMIT 1
> c.articles  ==> SELECT * FROM `articles` WHERE `articles`.`category_id` = 1


Category.with_('articles').first().articles ==> Collection (elment type is Article)
> SELECT * FROM `categories` LIMIT 1
> SELECT * FROM `articles` WHERE `articles`.`category_id` IN (1)


Article.first().tags ==> Collection (element type is Tag)
> a = Article.first()
> 
>     SELECT * FROM `articles` LIMIT 1
>
> a.tags  ==> 
> 
>     SELECT 
>         *, 
>         `article_tags`.`article_id` AS `pivot_article_id`, 
>         `article_tags`.`tag_id` AS `pivot_tag_id` 
>     FROM 
>         `app_tag` 
>     INNER JOIN 
>         `article_tags` 
>     ON 
>         `app_tag`.`id` = `article_tags`.`tag_id` 
>     WHERE 
>         `article_tags`.`article_id` = 1


Article.with_('tags').first().tags ==> Collection (element type is Tag)
>     SELECT * FROM `articles` LIMIT 1
>
>     SELECT 
>       *, 
>       `article_tags`.`article_id` AS `pivot_article_id`, 
>       `article_tags`.`tag_id` AS `pivot_tag_id` 
>     FROM 
>       `app_tag` 
>     INNER JOIN 
>       `article_tags` 
>     ON 
>       `app_tag`.`id` = `article_tags`.`tag_id` 
>     WHERE 
>       `article_tags`.`article_id` IN (1)'
>

