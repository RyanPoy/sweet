# Recordset

- Introduction
- Retrieving Results
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
- Order By
- Group By / Having 
- Page
- Inserts
- Updates
  - Increment & Decrement
- Deletes
- Locking
- Transaction

## Introduction
the basic database operate 

## Retrieving Results
db.records('users').all()

```
SELECT * FROM `users`
```

db.records('users').first()

```
> SELECT * FROM `users` limit 1
```

db.records('users').last()

```
SELECT * FROM `users`
```
> note: then get last record


### Aggregates
db.records('users').count()

```
SELECT COUNT(*) FROM `users`
```

db.records('orders').max('price')

```
SELECT MAX(`price`) FROM orders
```

> just support: count、max、min、avg、sum


## Selects
db.records('users').select('age').all()

```
SELECT `age` FROM `users`
```

db.records('users').select('age', 'name).all()

```
SELECT age, name FROM users
```

db.records('users').select('age').select('name').all()

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
db.records('users').join('posts', users__id="posts.user_id").all()

``` 
SELECT * FROM `users` INNER JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```  

If you would like to perform a "left join" instead of an "inner join", use the leftJoin method. The leftJoin method has the same signature as the join method:
            
### Left Join Clause
db.records('users').left_join('posts', on="users.id = posts.user_id").all()

``` 
SELECT * FROM `users` LEFT JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Right Join Clause
db.records('users').right_join('posts', on="users.id = posts.user_id").all()

```
SELECT * FROM `users` RIGHT JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Cross Join Clause
db.records('users').cross_join('posts', on="users.id = posts.user_id").all()

```
SELECT * FROM `users` CROSS JOIN `posts` ON `users`.`id` = `posts`.`user_id`
```

### Advanced Join Clauses
```
def complex(join):
    join.on('user.id=contacts.user_id').and_(user__id=10).or_(user__name='abc')
db.records('users').join('contacts', complex).all()
```

```
SELECT * FROM `users` INNER JOIN `contacts` ON `user`.`id`=`contacts`.`user_id` AND `user`.`id` = 10 OR `user`.`name` = 'abc'
```
Note：If you would like to use a "where" style clause on your joins, you may use the "and_" and "or_" methods on a join. Instead of comparing two columns, these methods will compare the column against a value

## Unions
```
db.records('users').where(first_name=None).union(
    db.records('users').where(last_name=None)
).all()
```

```
SELECT * FROM `users` WHERE first_name IS NULL UNION SELECT * FROM `users` WHERE last_name IS NULL 
```

## Where Clauses
### Basic where
db.records('users').where(name=None).all()

```
SELECT * FROM `users` WHERE `id` IS NULL 
```

db.records('users').where(name__not=None).all()

```
SELECT * FROM `users` WHERE `id` IS NOT NULL 
```

db.records('users').where(name__like='%Jim%').all()

```
SELECT * FROM `users` WHERE `id` LIKE '%Jim%'
```

db.records('users').where(name__not_like='%Jim%').all()

```
SELECT * FROM `users` WHERE `id` NOT LIKE '%Jim%'
```

db.records('users').where(id=10).all()

```
SELECT * FROM `users` WHERE `id` = 10 
```

db.records('users').where(id__not=10).all()

```
SELECT * FROM `users` WHERE `id` <> 10
```

db.records('users').where(id__lt=10).all()

```
SELECT * FROM `users` WHERE `id` < 10
```

db.records('users').where(id__lte=10).all()

```
SELECT * FROM `users` WHERE `id` <= 10
```

db.records('users').where(id__gt=10).all()

```
SELECT * FROM `users` WHERE `id` > 10
```

db.records('users').where(id__gte=10).all()

```
SELECT * FROM `users` WHERE `id` >= 10
```

db.records('users').where(id__bt=[1, 5]).all()

```
SELECT * FROM `users` WHERE `id` BETWEEN 1 AND 5
```


db.records('users').where(id__not_bt=[1, 5]).all()

```
SELECT * FROM `users` WHERE `id` NOT BETWEEN 1 AND 5
```


db.records('users').where(id=[1, 5]).all()

```
SELECT * FROM `users` WHERE `id` IN (1, 5)
```


db.records('users').where(id__not=[1, 5]).all()

```
SELECT * FROM `users` WHERE `id` NOT IN (1, 5)
```

### Parameter Grouping
```
db.records('users').where(id__not=[1, 5]).where(
	WhereClause().and_(name='jim').or_(name='lucy')
).all()
```

```
SELECT * FROM `users` WHERE `id` NOT IN (1, 5) AND ( `name` = 'jim' OR `name` = 'lucy' )
```

### Where Exists Clauses
```
users = db.records('users').where_exists(
    db.records('mobiles').where(name='iphone'),
    db.records('mobiles').where(name='aphone')
).all()
```

```
SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = 'iphone') AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = 'aphone')
```

## Order By
db.records('users').order_by('id')

```
SELECT * FROM `users` ORDER BY `id`
```

db.records('users').order_by('id', False)

```
SELECT * FROM `users` ORDER BY `id`
```

db.records('users').order_by('id', True)

```
SELECT * FROM `users` ORDER BY `id` DESC
```

## Group By / Having 
db.records('users').group_by('school_id').having(school_id__bt=[1, 100]).all()

```
SELECT * FROM `users` GROUP BY `school_id` HAVING `school_id` BETWEEN 1 AND 100
```


## Page
### Limit / Offset
db.records('users').limit(10).offset(5).all()

```
SELECT * FROM `users` LIMIT 10 OFFSET 5
```

### Paginator
db.records('users').page(2, 15).all()

```
SELECT * FROM `users` LIMIT 15 OFFSET 15
```

## Inserts

### single insert and get id
db.records('users').insert_getid(id=3, name='jim', age=23)

```
INSERT INTO `users` (`id`, `name`, `age`) VALUES (3, 'jim', 23)
```

### single insert and get how many insert successful
db.records('users').insert(id=3, name='jim', age=23)

```
INSERT INTO `users` (`id`, `name`, `age`) VALUES (3, 'jim', 23)
```

### multiple insert and get how many insert successful
```
db.records('users').insert([
	dict(id=3, name='jim', age=23),
	dict(id=5, name='lily', age=32),
])
```

```
INSERT INTO `users` (`id`, `name`, `age`) VALUES (3, 'jim', 23), (5, 'lily', 32)
```


## Updates
### Updating Columns
db.records('users').where(id__gt=10).update(age=30, gender='m')

```
UPDATE `users` SET `age` = 30, `gender` = 'm' WHERE id > 10
```


### Increment & Decrement
db.records('users').increment(age=10, score=20)

```
UPDATE `users` SET `age` = `age` + 10, `score` = `score` + 20
```

db.records('users').decrement(age=10, score=20)

```
UPDATE `users` SET `age` = `age` - 10, `score` = `score` - 20
```


## Deletes
### delete
db.records('users').delete()

```
DELETE `users`
```

### truncate
db.records('users').truncate()

```
TRUNCATE `users`
```

## Locking

### read_lock
```
db.records('users')
  .select('users.id', 'cars.name')
  .left_join('cars', 'users.id=cars.user_id')
  .where(car_id=10)
  .read_lock()
```

```
SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `car_id` = 10 LOCK IN SHARE MODE'
```


### write_lock

```
db.records('users')
  .select('user.id')
  .select('cars.name')
  .left_join('cars', 'users.id=cars.user_id')
  .where(car_id=10)
  .write_lock()
```

```
SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `car_id` = 10 FOR UPDATE
```


### Transaction
```
  with db.transction():
    db.records('users').insert([
      dict(id=3, name='jim', age=23),
      dict(id=5, name='lily', age=32),
    ])
```
