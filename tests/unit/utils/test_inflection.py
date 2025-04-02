import unittest

from sweet.utils.inflection import javaize, pluralize, pythonize, singularize


def test_pluralize(singular_and_plural):
    for singular, plural in singular_and_plural:
        assert pluralize(singular) == plural


def test_singularize(singular_and_plural):
    for singular, plural in singular_and_plural:
        assert singularize(plural) == singular


def test_javaize():
    assert 'User', javaize('User')
    assert 'TestModelForTableName' == javaize('TestModelForTableName')
    assert 'TestModelForTableName' == javaize('Test_model_for_Table_Name')
    assert 'TestModelForTableName' == javaize('Test_model_forTableName')
    assert 'TestModelForTableName' == javaize('_Test_model_forTableName')
    assert 'TestModelForTableName' == javaize('1002_test_model_forTableName')


def test_pythonize():
    assert 'user', pythonize('User')
    assert 'test_model_for_table_name' == pythonize('TestModelForTableName')
    assert 'test_model_for_table_name' == pythonize('Test_model_for_Table_Name')
    assert 'test_model_for_table_name' == pythonize('Test_model_forTableName')
    assert 'create_user' == pythonize('CreateUser')


if __name__ == '__main__':
    unittest.main()
