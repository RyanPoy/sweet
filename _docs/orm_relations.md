# ORM
=======

- belongs_to
  - create
  - save
  - update
  - delete
  - query
- has_many
  - create
  - save
  - update
  - delete
  - query
- has_many with through
  - create
  - save
  - update
  - delete
  - query
- has_one
  - create
  - save
  - update
  - delete
  - query
- has_one with through
  - create
  - save
  - update
  - delete
  - query

- `has_and_belongs_to_many`
  - create
  - save
  - update
  - delete
  - query

----

## belongs_to

```
-- create tables

  create table users(
    create table users (
    id int auto_increment primary key,
    name varchar(32) not null default '',
    age int not null default 20
  );

  create table mobiles (
    id int auto_increment primary key,
    name varchar(32) not null default '',
    user_id int not null,
    foreign key (user_id) references users(id)
  );

--------------------------------------------------------
# model define
  
  from sweet.orm import Model
  from sweet.relations import *

  class User(Model):
    pass

  class Mobile(Model):
    belongs_to(User, name='user')  # name does not required. should be auto set
```

### create

```
  u = User.create(name="jon", age=20)
  Mobile.create(name="Nokia", user=u)
  Mobile.create(name="Nokia", user_id=u.id)
```

### save

```
  u = User(name="jon", age=20).save()
  Mobile(name="Nokia", user=u).save()
  Mobile(name="Nokia", user_id=u.id).save()
```

### update

```
  u1 = User(name="jon", age=20).save()
  u2 = User(name="lily", age=30).save()
  m = Mobile(name="Nokia", user=u1).save()
  
  m.update(user=u2)
  m.update(user_id=u2.id)
  
  m.user = u2
  m.save()
  
  m.user_id = u2.id
  m.save()

```

### delete

```
  u = User(name="jon", age=20).save()
  m = Mobile(name="Nokia", user=u).save()
  m.delete()
```

### query

```
  u = User.create(name="jon", age=20)
  m = Mobile.create(name="Nokia", user_id=u.id)
  m.user  # find it from database
```

## has_many

```
-- create tables

  create table users(
    create table users (
    id int auto_increment primary key,
    name varchar(32) not null default '',
    age int not null default 20
  );

  create table mobiles (
    id int auto_increment primary key,
    name varchar(32) not null default '',
    user_id int not null,
    foreign key (user_id) references users(id)
  );

--------------------------------------------------------
# model define
  
  from sweet.orm import Model
  from sweet.relations import *

  class User(Model):
    has_many(Mobile)   #  if Mobile and User in same python file. 
                       #  you can coding that: 
                       # 
                       #  class User(Model):
                       #     has_many('pkg1.pkg2.Mobile')
                       #
                       #  note: pkg1, pkg2 are Mobile packages

  class Mobile(Model):
    belongs_to(User)  # name does not required. should be auto set
```

- create

```
  u = User()
```

- save
- update
- delete
- query

## has_many with through
- create
- save
- update
- delete
- query

## has_one
- create
- save
- update
- delete
- query

## has_one with through
- create
- save
- update
- delete
- query

## `has_and_belongs_to_many`
- create
- save
- update
- delete
- query
