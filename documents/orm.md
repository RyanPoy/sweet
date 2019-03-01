# ORM

## list
- Introduction
- define
- save
- update
- delete

==
- Introduction
the orm which implement ActiveRecord

- define

```
class User(Model):
  pass
```

User would be add column named created_at, updated_at, id auto. 
And created_at, updated_at are datetime type, id is a integer auto increament

if you don't create created_at and updated_at, you can set __timestamp__ is False in User.
Just like this:

```
class User(Model):
  __timestamp__ = False

```


- save / create

```  
User(name='jim', age=20).save()

User.create(name='jim', age=20)
```

- update

```
u = User.where(name="jim")
u.update(name="lily", age=20)

u = User.where(name='jim')
u.name = 'lily'
u.age = 20
u.save()
```







==
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

