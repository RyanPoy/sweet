# -*- coding:utf-8 -*-
from ..utils import singularize_of, pluralize_of, ColumnNotInColumns


def owner_attr_for_has_one_and_has_belongs_to(self):
    """ for has_one and belongs_to
    """
    if self._owner_attr:
        return self._owner_attr
    if self.owner is None:
        return None
    from ..record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    target = self.target
    if target is None:
        return None
    self._owner_attr = singularize_of(target.table_name)
    return self._owner_attr


def owner_attr_for_has_many(self):
    """ for has_many
    """
    if self._owner_attr:
        return self._owner_attr
    if self.owner is None:
        return None
    from ..record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    target = self.target
    if target is None:
        return None
    self._owner_attr = pluralize_of(target.table_name)
    return self._owner_attr


def foreign_key_for_belongs_to(self):
    """Phone belongs to User
    owner_column must be user_id
    """
    if self._foreign_key:
        return self._foreign_key
    if self.owner is None:
        return None
    from ..record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    target = self.target
    if target is None:
        return None
    foreign_key = singularize_of(target.table_name) + '_id'
    if not self.owner.has_column(foreign_key):
        raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.owner.__name__))
    self._foreign_key = foreign_key
    return self._foreign_key



def foreign_key_for_has_one_and_has_many(self):
    """Phone belongs to User
    owner_column must be user_id
    """
    if self._foreign_key:
        return self._foreign_key
    if self.owner is None:
        return None
    from ..record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    owner = self.owner
    if owner is None:
        return None
    foreign_key = singularize_of(owner.table_name) + '_id'
    if not self.target.has_column(foreign_key):
        raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
    self._foreign_key = foreign_key
    return self._foreign_key
