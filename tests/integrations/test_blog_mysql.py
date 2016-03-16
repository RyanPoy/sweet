#coding: utf8
from sweet.database import MySQL, DatabaseManager
from sweet.tests.integrations.blog import *
from sweet.record import ActiveRecord
import unittest


class BlogMySQLTest(unittest.TestCase):
    
    def setUp(self):
        db = MySQL('test', user='root', password='', show_sql=True)
        db.execute(drop_db_sql)
        db.execute(create_db_sql)
        db.close()

        db = MySQL(database, user='root', password='', show_sql=True)
        db.execute(create_table_sql)
        db.close()

        ActiveRecord.__dbmanager__ = DatabaseManager({
            'driver': 'mysql',
            'host': 'localhost',
            'port': 3306,
            'database': database,
            'user': 'root',
            'password': '',
            'show_sql': True,
        })

    def tearDown(self):
        db = MySQL('test', user='root', password='', show_sql=True)
        db.execute(drop_db_sql)
        db.close()
        ActiveRecord.__dbmanager__.close_all_connection()

    def test_business(self):
        # initial the categories
        c_tech = Category.create(name=u'科技')
        c_sport = Category(name=u'体育').save()
        c_other = Category.create(name=u'其它')
        self.assertEqual(3, len(Category.all()))
        
        # initial users        
        User.create(name='admin', password="123123") # register ok
        self.assertEquals(None, User.where(name="admin", password="err_password").first()) # login with err password should be return None
        admin = User.where(name="admin", password="123123").first() # login ok
        print '*'*10, admin, type(admin)
        self.assertTrue(isinstance(admin, User))

        # initial articles 
        Article.create(title=u'微软开数码咖啡厅', content=u'''大玻璃窗，精心的装饰和屏幕，真的有很多屏幕。智能手机、平板、电视机...这便是微软的“数码咖啡厅”。
        微软公司的这个想法只是作为另一全面计划的一部分，公司希望借此来重塑形象﹑恢复其科技领袖的地位。该场所有个类似苹果商店的开放区，但是这的商品并不出售。该场所配有椅子，桌子和沙发，
        为大家提供了各种Windows电子设备。最新款的操作系统Windows 8专为触摸屏设备提供，给消费者带来了更好的体验和不一样的享受。这个微软咖啡厅看来是个绝佳的地方来体验该新系统。
        微软公司希望能重新找回之前失去的市场。''', category_id=c_tech.id, user_id=admin.id) # create article

        Article.create(title=u'互联网公司火拼基金销售', content=u'''余额宝和百发分别对接的是天弘基金的天弘增利宝和华夏基金的华夏现金增利货币E，
        都属于货币基金，货币基金的盈利模式主要由管理费收入、销售服务费收入、手续费收入、利息收入、投资收益收入等构成。其中由于货币基金整体收益不高，所以这部分收入所占不多，
        占比最大的收入来自销售服务收入。而具体到支付宝和天弘基金而言，按现在天弘基金1000亿元的销售规模，占总额0.3%的管理费将会归基金公司所有，即天弘从中可获得3亿元管理费收入。
        对支付宝这端，天弘会以技术或者渠道管理费等由头与支付宝进行分成，双方并未公布具体分成的比例。但据一位知情人士透露，此前，数米基金网等第三方基金销售网站代销基金，
        基金公司需要网站分成30%-40%的管理费。''', category_id=c_tech.id, user_id=admin.id)  # create article
        
        Article.create(title=u'湖人正式宣布与科比续约', content=u'''在续约发布会上，湖人方面表示，这份新的合同将确保科比终老湖人。“我们一直在说，我们最紧迫的事情，
        就是科比能在湖人结束自己的职业生涯。在这份续约合同签完之后，我们觉得这（科比终老湖人）肯定没有问题了。”库普切克说，“我们终于把世界上最好的球员留在了湖人。”
        在与湖人完成续约之后，1996年便进入联盟的科比为紫金军效力的时间（也是职业生涯的时间）将至少达到20年。“在同一支球队取得如此辉煌的成就，这绝对是史无前例的。”库普切克表示。
        在效力湖人期间，科比帮助球队收获5个总冠军，其中包括2000-2002年的三连冠，以及2009和2010年的两连冠。尽管已经拿到5个总冠军，但科比对冠军的渴望一直没有停止，他仍渴望能获得生涯的第6个冠军。
        而在ESPN专家马卡兹看来，这一次续约让科比再次夺冠的机会大大增加。''', category_id=c_sport.id, user_id=admin.id)

        # self.assertTrue(isinstance(admin.articles, Collection))
        
        # self.assertEqual(3, admin.articles.count())

        # self.assertEqual(u'微软开数码咖啡厅', admin.articles[0].title)
        # self.assertEqual(u'互联网公司火拼基金销售', admin.articles[1].title)
        # self.assertEqual(u'湖人正式宣布与科比续约', admin.articles[2].title)
        
        # self.assertEqual(1, len(c_sport.articles))
        # self.assertEqual(u'湖人正式宣布与科比续约', c_sport.articles[0].title)
        # self.assertEqual(2, len(c_tech.articles))
        # self.assertEqual(u'微软开数码咖啡厅', c_tech.articles[0].title)
        # self.assertEqual(u'互联网公司火拼基金销售', c_tech.articles[1].title)


        
if __name__ == '__main__':
    unittest.main()
