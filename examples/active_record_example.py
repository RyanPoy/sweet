#coding: utf8
import pyrails
"""
you must create database first:
    create database simple_blog default charset utf8;

"""

DEBUG = True
DATABASE = dict (
    engine      = 'mysql',
    database    = 'simple_blog',
    user        = 'root', 
    password    = '', 
    host        = 'localhost', 
    port        =  3306, 
    charset     =  'utf8',
)

from pyrails.db import get_database
pyrails.get_database = lambda db=DATABASE, debug=DEBUG: get_database(db, debug)


from pyrails.activerecord import *
from pprint import pprint


# simple blog 
def prepare_db():
    pyrails.get_database(debug=False).execute_rowcount(
        '''create table users (
            id integer auto_increment primary key,
            name varchar(32),
            age integer,
            gender varchar(4)
        );'''
    )
    pyrails.get_database(debug=False).execute_rowcount(
        '''create table categories(
            id integer auto_increment primary key,
            name varchar(32)
        );'''
    )
    pyrails.get_database(debug=False).execute_rowcount(
        '''create table posts (
            id integer auto_increment primary key,
            title varchar(128),
            content text,
            user_id integer,
            foreign key (user_id) references users(id)
        );'''
    )
    pyrails.get_database(debug=False).execute_rowcount(
        '''create table categories_posts (
            id integer auto_increment primary key,
            category_id integer,
            post_id integer,
            foreign key (category_id) references categories(id),
            foreign key (post_id) references posts(id)
       );'''
    )


def drop_db():
    pyrails.get_database(debug=False).execute_rowcount('drop table categories_posts')
    pyrails.get_database(debug=False).execute_rowcount('drop table posts')
    pyrails.get_database(debug=False).execute_rowcount('drop table categories')
    pyrails.get_database(debug=False).execute_rowcount('drop table users')


class User(ActiveRecord):
    has_many('pyrails.examples.active_record_example.Post')
    
    validates_presence_of('name', msg=u'不能为空')
    validates_length_of('name', minimum=2, maximum=8, msg=u'长度在2-8')
    validates_uniqueness_of('name', msg=u'已经存在')
    validates_numericality_of('age', greater_than=10, less_than=80, msg=u'必须是数字, 且在10~80之间')
    validates_inclusion_of('gender', in_values=[u'男', u'女'], msg=u'请选择正确的性别')

    def __str__(self):
        return (u'User[id=%s|name=%s|age=%s|gender=%s]' % (self.id, self.name, self.age, self.gender)).encode('utf8')


class Category(ActiveRecord):
    has_and_belongs_to_many('pyrails.examples.active_record_example.Post')

    validates_of('name', presence=dict(msg=u'不能为空'),
                         uniqueness=dict(msg=u'已经存在'),
                         length=dict(minimum=2, maximum=16, msg=u'长度在2-16'))

    def __str__(self):
        return (u'Category[id=%s|name=%s]' % (self.id, self.name)).encode('utf8')


class Post(ActiveRecord):
    belongs_to(User)
    has_and_belongs_to_many(Category)

    validates_presence_of(['title', 'content'], msg=u'不能为空')
    validates_length_of('title', minimum=4, maximum=32, msg=u'长度在4-32')
    validates_length_of('content', minimum=4, maximum=2048, msg=u'长度在4-2048')

    def __str__(self):
        return (u'Post[id=%s|title=%s]' % (self.id, self.title)).encode('utf8')


def example():
    print '\n***** create category which named: 体育'
    Category.create(name=u'体育')
    print '\n***** create category which named: 数码'
    Category.create(name=u'数码')

    # c = Category(name=u'体育')
    # if not c:
    #     print 'Error:'
    #     pprint(c.errors)

    print '\n***** find all categories'
    print [ str(c) for c in Category.all ]
    print '\n***** find first category'
    print Category.first
    print '\n***** find last category'
    print Category.last
    print '\n***** calc category count'
    print Category.count()
    print '\n***** find category which named 体育'
    print Category.where(name=u'体育').all

    print '\n***** create user which named pengyi'
    User.create(name='pengyi', age=33, gender=u'男')
    print '\n***** create user which named ryan'
    User.create(name='ryan', age=24, gender=u'男')
    print '\n***** create user which named py'
    User.create(name='py', age=12, gender=u'男')
    print "\n***** calc sum of user's age"
    print User.sum('age')
    print '\n***** find user which named py'
    print User.find_by_name('py')

    
    print '\n***** find user which id is 1'
    u = User.find(1)

    print '\n***** create post which user_id is 1 and category name is 体育'
    u.posts.create(title=u'湖人正式宣布与科比续约', content=u'''在续约发布会上，湖人方面表示，这份新的合同将确保科比终老湖人。“我们一直在说，我们最紧迫的事情，
        就是科比能在湖人结束自己的职业生涯。在这份续约合同签完之后，我们觉得这（科比终老湖人）肯定没有问题了。”库普切克说，“我们终于把世界上最好的球员留在了湖人。”
        在与湖人完成续约之后，1996年便进入联盟的科比为紫金军效力的时间（也是职业生涯的时间）将至少达到20年。“在同一支球队取得如此辉煌的成就，这绝对是史无前例的。”库普切克表示。
        在效力湖人期间，科比帮助球队收获5个总冠军，其中包括2000-2002年的三连冠，以及2009和2010年的两连冠。尽管已经拿到5个总冠军，但科比对冠军的渴望一直没有停止，他仍渴望能获得生涯的第6个冠军。
        而在ESPN专家马卡兹看来，这一次续约让科比再次夺冠的机会大大增加。''', category=Category.find_by_name(u'体育'))

    print '\n***** create post which user_id is 1 and category name is 数码'
    u.posts.create(title=u'微软开数码咖啡厅', content=u'''大玻璃窗，精心的装饰和屏幕，真的有很多屏幕。智能手机、平板、电视机...这便是微软的“数码咖啡厅”。
        微软公司的这个想法只是作为另一全面计划的一部分，公司希望借此来重塑形象﹑恢复其科技领袖的地位。该场所有个类似苹果商店的开放区，但是这的商品并不出售。该场所配有椅子，桌子和沙发，
        为大家提供了各种Windows电子设备。最新款的操作系统Windows 8专为触摸屏设备提供，给消费者带来了更好的体验和不一样的享受。这个微软咖啡厅看来是个绝佳的地方来体验该新系统。
        微软公司希望能重新找回之前失去的市场。''', category=Category.where(name=u'数码').first)

    print '\n***** find post which belongs to user 1'
    print [ str(post) for post in u.posts ]

    print '\n***** find post which belongs to category named 体育'
    print [ str(post) for post in Category.find_by_name(u'体育').posts ]
    
    print '\n***** find post which belongs to category named 数码'
    print [ str(post) for post in Category.find_by_name(u'数码').posts ]


if __name__ == '__main__':
    try:
        drop_db()
        prepare_db()
        example()
    finally:
        # drop_db()
        pass

    print 'done'
