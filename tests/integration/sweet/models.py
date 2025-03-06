# # 🌟 has_many_and_belongs_to (Many-to-Many) 需要中间表
# class UsersRoles(BaseModel):
#     __table__ = UsersRolesTable

# user_roles = Table(
#     'user_roles', Base.metadata,
#     Column('user_id', Integer, ForeignKey('users.id'), primary_key=True),
#     Column('role_id', Integer, ForeignKey('roles.id'), primary_key=True)
# )

# 🌟 has_many_and_belongs_to :through 需要中间模型
# class Membership(BaseModel):
#     __tablename__ = 'memberships'
#     id = Column(Integer, primary_key=True)
#     user_id = Column(Integer, ForeignKey('users.id'))
#     team_id = Column(Integer, ForeignKey('teams.id'))
#
# # 🌟 Users 表
# class User(BaseModel):
#     __tablename__ = 'users'
#     id = Column(Integer, primary_key=True)
#     name = Column(String)
#
#     # has_one
#     profile = relationship("Profile", uselist=False, back_populates="user")
#
#     # has_many
#     posts = relationship("Post", back_populates="user")
#
#     # has_many_and_belongs_to (Many-to-Many)
#     roles = relationship("Role", secondary=user_roles, back_populates="users")
#
#     # has_many :through (Users -> Membership -> Teams)
#     teams = relationship("Team", secondary="memberships", back_populates="users")
#
#     # has_one :through (User -> Profile -> Address)
#     address = relationship("Address", secondary="profiles", uselist=False)
#
#     # belongs_to :through (User -> Membership -> Team -> Organization)
#     organizations = relationship("Organization", secondary="teams", back_populates="users")
#
# # 🌟 Profile 表 (has_one)
# class Profile(BaseModel):
#     __tablename__ = 'profiles'
#     id = Column(Integer, primary_key=True)
#     user_id = Column(Integer, ForeignKey('users.id'))
#     address_id = Column(Integer, ForeignKey('addresses.id'))  # for has_one :through
#     user = relationship("User", back_populates="profile")
#
# # 🌟 Address 表 (has_one :through)
# class Address(BaseModel):
#     __tablename__ = 'addresses'
#     id = Column(Integer, primary_key=True)
#     city = Column(String)
#
# # 🌟 Posts 表 (has_many)
# class Post(BaseModel):
#     __tablename__ = 'posts'
#     id = Column(Integer, primary_key=True)
#     title = Column(String)
#     user_id = Column(Integer, ForeignKey('users.id'))
#     user = relationship("User", back_populates="posts")
#
# # 🌟 Roles 表 (has_many_and_belongs_to)
# class Role(BaseModel):
#     __tablename__ = 'roles'
#     id = Column(Integer, primary_key=True)
#     name = Column(String)
#     users = relationship("User", secondary=user_roles, back_populates="roles")
#
# # 🌟 Teams 表 (has_many :through)
# class Team(BaseModel):
#     __tablename__ = 'teams'
#     id = Column(Integer, primary_key=True)
#     name = Column(String)
#     users = relationship("User", secondary="memberships", back_populates="teams")
#
# # 🌟 Organizations 表 (belongs_to :through)
# class Organization(BaseModel):
#     __tablename__ = 'organizations'
#     id = Column(Integer, primary_key=True)
#     name = Column(String)
#     teams = relationship("Team", back_populates="organization")
#     users = relationship("User", secondary="teams", back_populates="organizations")