# ORM Relations

- belongs_to
  - create
  - save
  - update
  - delete
  - query
- has_many
  - query
- has_many with through
  - query
  - dissociate
- has_one
  - query
- has_one with through
  - query
  - dissociate

- `has_and_belongs_to_many`
  - associate
  - dissociate
  - query

- Relation self
- N + 1
  - belongs_to
  - has_many
  - has_one
  - has_many with through
  - has_and_belongs_to_many
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
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class User(Model):
    has_many(Mobile)   #  if Mobile and User in same python file. 
                       #  you can coding that: 
                       # 
                       #  class User(Model):
                       #     has_many('demo.Mobile')
                       #
                       #  note: demo is package of Mobile

  class Mobile(Model):
    belongs_to(User)  # name does not required. should be auto set
```

### query

```
  u = User.create(name="jon", age=20)
  Mobile.create(name="Nokia", user=u)
  Mobile.create(name="IPhone", user=u)
  Mobile.create(name="Vivo", user=u)
  
  u.mobiles.all()
  u.mobiles.first()

```

## has_many with through

```
-- create tables

  create table students (
    id int auto_increment primary key,
    name varchar(32) not null default ''
  );

  create table courses (
    id int auto_increment primary key,
    name varchar(32) not null default ''
  );

  create table scores (
    id int auto_increment primary key,
    student_id int not null,
    course_id int not null,
    value int not null default 0,
    foreign key (student_id) references students(id),
    foreign key (course_id) references courses(id)    
  );

--------------------------------------------------------
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class Score(Model):
    belongs_to('demo.Student')
    belongs_to('demo.Course')


  class Student(Model):
    has_many(Score)
    has_many('demo.Course', through=Score)


  class Course(Model):
    has_many(Score)
    has_many(Student, through=Score)
```

### query

```
  s1 = Student.create(name='lily')
  s2 = Student.create(name='jon')

  c1 = Course.create(name='math')
  c2 = Course.create(name='sport')

  Score.create(student=s1, course=c1, value=100)
  Score.create(student=s1, course=c2, value=90)
  Score.create(student=s2, course=c1, value=95)
  Score.create(student=s2, course=c2, value=98)
  
  s1.courses.all()
  s2.courses.all()

  c1.students.all()
  c2.students.all()
```

### dissociate

```
  s1 = Student.create(name='lily')
  s2 = Student.create(name='jon')

  c1 = Course.create(name='math')
  c2 = Course.create(name='sport')

  Score.create(student=s1, course=c1, value=100)
  Score.create(student=s1, course=c2, value=90)
  Score.create(student=s2, course=c1, value=95)
  Score.create(student=s2, course=c2, value=98)
  
  Score.where(student_id=s1.id, course_id=c1.id).delete()
  s1.dissociate_with_scores(c1, c2)  # dissociate_with_scores is a dynamic method. 
  c2.dissociate_with_students(s1, s2)  # dissociate_with_students is a dynamic method. 
  
```

## has_one

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
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class User(Model):
    has_one(Mobile)


  class Mobile(Model):
    belongs_to(User)  # name does not required. should be auto set
```

### query

```
  u = User.create(name="jon", age=20)
  Mobile.create(name="Nokia", user=u)
  
  u.mobile
```


## has_one with through

```
-- create tables

  create table students (
    id int auto_increment primary key,
    name varchar(32) not null default ''
  );

  create table courses (
    id int auto_increment primary key,
    name varchar(32) not null default ''
  );

  create table scores (
    id int auto_increment primary key,
    student_id int not null,
    course_id int not null,
    value int not null default 0,
    foreign key (student_id) references students(id),
    foreign key (course_id) references courses(id)    
  );

--------------------------------------------------------
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class Score(Model):
    belongs_to('demo.Student')
    belongs_to('demo.Course')


  class Student(Model):
    has_one(Score)
    has_one('demo.Course', through=Score)


  class Course(Model):
    has_one(Score)
    has_one(Student, through=Score)
```

### query

```
  s1 = Student.create(name='lily')
  s2 = Student.create(name='jon')

  c1 = Course.create(name='math')
  c2 = Course.create(name='sport')

  Score.create(student=s1, course=c1, value=100)
  Score.create(student=s2, course=c2, value=98)
  
  s1.course
  s2.course

  c1.student
  c2.student
```

### dissociate

```
  s1 = Student.create(name='lily')
  s2 = Student.create(name='jon')

  c1 = Course.create(name='math')
  c2 = Course.create(name='sport')

  Score.where(student_id=s1.id, course_id=c1.id).delete()
  s2.dissociate_with_score(c1)  # dissociate_with_score is a dynamic method. 
  c1.dissociate_with_student(s1)  # dissociate_with_student is a dynamic method. 
  
```


## `has_and_belongs_to_many`

```
-- create tables

  create table articles (
    id int auto_increment primary key,
    title varchar(64) not null,
    content text
  );

  create table tags (
    id int auto_increment primary key,
    name varchar(32) not null
  );

  create table articles_tags (
    id int auto_increment primary key,
    article_id int not null,
    tag_id int not null,
    foreign key (article_id) references articles(id),
    foreign key (tag_id) references tags(id)
  );

--------------------------------------------------------
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class Article(Model):
    has_and_belongs_to_many('demo.Tag')


  class Tag(Model):
    has_and_belongs_to_many(Article)

```

### associate

```
  t1 = Tag.create(name='cartoon')
  t2 = Tag.create(name='movie')

  a1 = Article.create(title='title-1', content='content—1')
  a2 = Article.create(title='title-2', content='content—2')
  a3 = Article.create(title='title-3', content='content—3')
  a4 = Article.create(title='title-4', content='content—4')

  t1.associate_with_articles(a1)               # associate_with_articles is a dynamic method
  t1.associate_with_articles(a2, a3, a4)       # associate_with_articles is a dynamic method
  a1.associate_with_tags(t1, t2)               # associate_with_tags is a dynamic method
```

### dissociate

```
  t1 = Tag.create(name='cartoon')
  t2 = Tag.create(name='movie')

  a1 = Article.create(title='title-1', content='content—1')
  a2 = Article.create(title='title-2', content='content—2')
  a3 = Article.create(title='title-3', content='content—3')
  a4 = Article.create(title='title-4', content='content—4')

  t1.associate_with_articles(a1)               # associate_with_articles is a dynamic method
  t1.associate_with_articles(a2, a3, a4)       # associate_with_articles is a dynamic method
  a1.associate_with_tags(t1, t2)               # associate_with_tags is a dynamic method
  
  t1.dissociate_with_articles(a1, a2, a3, a4)  # dissociate_with_articles is a dynamic method
  a1.dissociate_with_tags(t1, t2)              # dissociate_with_tags is a dynamic method
```

### query

```
  t = Tag.first()
  t.articles.all()
  
  a = Article.find(1)
  a.tags.all()
```

## Relation self

```
-- create tables

  create table categories (
    id int auto_increment primary key,
    name varchar(32) not null default '',
    parent_id int default null,
    index(parent_id)
  );

--------------------------------------------------------
# model define: demo.py
  
  from sweet.orm import Model
  from sweet.relations import *

  class Category(Model):
    has_many('demo.Category', name='children', fk='parent_id')
    belongs_to('demo.Category', name='parent', fk='parent_id')
```

### create

```
  c_root = Category.create(name="category-root")
  c_1 = Category.create(parent=c_root, name='category-1')
  c_1_1 = Category.create(parent=c_1, name='category-1-1')
  c_1_2 = Category.create(parent=c_1, name='category-1-2')

  c_2 = Category.create(parent=c_root, name='category-1')
  c_2_1 = Category.create(parent=c_2, name='category-2-1')
  c_2_2 = Category.create(parent=c_2, name='category-2-2')
```

### update

```
  c_root = Category.create(name="category-root")
  c_1 = Category.create(parent=c_root, name='category-1')
  c_1_1 = Category.create(parent=c_1, name='category-1-1')
  c_1_2 = Category.create(parent=c_1, name='category-1-2')

  c_2 = Category.create(parent=c_root, name='category-1')
  c_2_1 = Category.create(parent=c_2, name='category-2-1')
  c_2_2 = Category.create(parent=c_2, name='category-2-2')
  
  c_2_1.parent = c_1
  c_2_1.save()

  c_2_2.update(parent=c_1)
```

### query

```
  c_root = Category.create(name="category-root")
  
  c_1 = Category.create(parent=c_root, name='category-1')
  c_1_1 = Category.create(parent=c_1, name='category-1-1')
  c_1_2 = Category.create(parent=c_1, name='category-1-2')

  c_2 = Category.create(parent=c_root, name='category-1')
  c_2_1 = Category.create(parent=c_2, name='category-2-1')
  c_2_2 = Category.create(parent=c_2, name='category-2-2')

  children = c_root.children.all()
  children = children[0].children.all()
  children = children[1].children.all()
```


## N + 1 

You can use `include` method to optimizate the N + 1 problem

### `belongs_to` 、`has_one`、`has_many`

```
# model define
  
  from sweet.orm import Model
  from sweet.relations import *

  class User(Model):
    has_many('demo.Mobile')
    has_one('demo.Car')

  class Mobile(Model):
    belongs_to(User, name='user')

  class Car(Model):
    belongs_to(User)

  ######### N + 1 Example #########
  for m in Mobile.all(): # N + 1
    print (m.user)

  for u in User.all(): # N + 1
    print (u.car)
    print (u.mobiles.all())

  ######### Solution to N + 1  #########
  for m in Mobile.include("user").all(): # use include
    print (m.user)

  for u in User.include('car', 'mobiles').all(): # user include
    print (u.car)
    print (u.mobiles.all())
```

> Note:
> use include method should return a Collection. 

```
u = User.first()
u.mobiles # return a Recordset

u = User.include('mobiles').first()
u.mobiles # return a Collection
```

### `has_one` with through、`has_many` with through

```
  from sweet.orm import Model
  from sweet.relations import *

  class Score(Model):
    belongs_to('demo.Student')
    belongs_to('demo.Course')


  class Student(Model):
    has_many(Score)
    has_many('demo.Course', through=Score)


  class Course(Model):
    has_many(Score)
    has_many(Student, through=Score)
    
  for s in Student.include("courses").all():
    print (s.courses.all())
```

> Note
> 
> `has_one` with through looks like `has_many` with through
> 


### `has_and_belongs_to_many`

```
  from sweet.orm import Model
  from sweet.relations import *

  class Article(Model):
    has_and_belongs_to_many('demo.Tag')


  class Tag(Model):
    has_and_belongs_to_many(Article)

  for t in Tag.include('articles').all():
    print (t.articles.all())
```
