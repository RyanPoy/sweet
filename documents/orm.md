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

