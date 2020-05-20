# ORM Basic

- Introduction
- Define
- Create
  - Create single model
  - Create multiple models
- Update
  - Update single model
  - Update multiple models
- Save
- Delete
  - Delete single model
  - Delete multiple models
- Retrieving Modles
  - Aggregates
- Transaction

----

## Introduction
the orm which implement ActiveRecord

```
  -- create the table
  
  create table users (
    id int auto_increment primary key ,
    name varchar(32) not null default '',
    age int not null default 20
  );
```

## Define

```
  class User(Model):
    pass
```

User would be add column named `created_at`, `updated_at`, `id` auto. And `created_at`, `updated_at` are datetime type, id is a integer auto increament

if you won't create `created_at` and `updated_at`, you can set `__timestamp__` is False in User. Just like this:

```
  class User(Model):
    __timestamp__ = False

```

## Create
### Create single model
```
  User.create(name='jim', age=25)
```

### Create multiple models
```
  User.create_all(
    dict(name='jim', age=25),
    dict(name='jon', age=35),
    dict(name='lily', age=20),
  )
```

## Update
### Update single model
```
  u = User.find(1)   # find the user which id = 1
  u.update(name="lily", age=20)
```

### Update multiple models
```
  User.update_all(name='lily', age=20) # udpate all users set name = 'lily' and age = 20
```

## Save
```
  u = User(name='jim', age=25)
  u.save() # will be create a model

  u = User.find(1)
  u.name = 'jon'
  u.age = 30
  u.save() # will be update
```


## Delete
### Delete single model
```
  u = User.find(1)
  u.delete()
```

### Delete multiple models
```
  User.delete_all()  # delete all users
  User.delete_all(age=20) # delete all users which age = 20
```

## Retrieving Modles
```
  User.first()
  User.last()
  User.all()
  User.where(name='jon').first()
  User.where(age__lt=30).all()
```

### Aggregates
```
  User.count()
  User.max('age')
  User.min('age')
  User.avg('age')
  User.sum('age')
```
> just support: count、max、min、avg、sum


### Transaction

- atomic

```
  from sweet.orm import atomic

  @atomic
  def delete():
    User.first().delete()
```

- manual

```
  from sweet.orm import atomic

  with User.transaction() as t:
    
    User.first().delete()
    t.commit()

    User.delete_all()
    t.rollback()
```
