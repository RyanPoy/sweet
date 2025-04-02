from sweet.model.columns import CharColumn, IntColumn
from sweet.model import Model


class UserRoles(Model):
    user_id = IntColumn()  # ForeignKey('users.id'), pk=True),
    role_id = IntColumn()  # ForeignKey('roles.id'), pk=True)


class Membership(Model):
    """ has_many_and_belongs_to :through 需要中间模型 """
    id = IntColumn(pk=True)
    user_id = IntColumn()  # ForeignKey('users.id')
    team_id = IntColumn()  # ForeignKey('teams.id')


class Profile(Model):
    """ Profile 表 (has_one) """
    id = IntColumn(pk=True)
    user_id = IntColumn()  # ForeignKey('users.id')
    address_id = IntColumn()  # ForeignKey('addresses.id') # for has_one :through
    # user = relationship("User", back_populates="profile")


class Address(Model):
    """ Address 表 (has_one :through) """
    id = IntColumn(pk=True)
    city = CharColumn(length=32)


class Post(Model):
    """ Posts 表 (has_many) """
    id = IntColumn(pk=True)
    title = CharColumn()
    user_id = IntColumn()  # ForeignKey('users.id')
    # user = relationship("User", back_populates="posts")


class Role(Model):
    """ Roles 表 (has_many_and_belongs_to) """
    id = IntColumn(pk=True)
    name = CharColumn()
    # users = relationship("User", secondary=user_roles, back_populates="roles")


class Team(Model):
    """ Teams 表 (has_many :through) """
    id = IntColumn(pk=True)
    name = CharColumn()
    # users = relationship("User", secondary="memberships", back_populates="teams")


class Organization(Model):
    """ Organizations 表 (belongs_to :through) """
    id = IntColumn(pk=True)
    name = CharColumn()
    # teams = relationship("Team", back_populates="organization")
    # users = relationship("User", secondary="teams", back_populates="organizations")


class User(Model):
    """ Users 表 """
    id = IntColumn(pk=True)
    name = CharColumn()

    # # has_one
    # profile = relationship("Profile", uselist=False, back_populates="user")
    #
    # # has_many
    # posts = relationship("Post", back_populates="user")
    #
    # # has_many_and_belongs_to (Many-to-Many)
    # roles = relationship("Role", secondary=user_roles, back_populates="users")
    #
    # # has_many :through (Users -> Membership -> Teams)
    # teams = relationship("Team", secondary="memberships", back_populates="users")
    #
    # # has_one :through (User -> Profile -> Address)
    # address = relationship("Address", secondary="profiles", uselist=False)
    #
    # # belongs_to :through (User -> Membership -> Team -> Organization)
    # organizations = relationship("Organization", secondary="teams", back_populates="users")


async def create_tables_for_model(driver, sqls):
    for sql in sqls:
        await driver.execute(sql)

async def delete_all_models(driver, sqls):
    for sql in sqls:
        await driver.execute(sql)
