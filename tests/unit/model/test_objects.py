import unittest

from sweet.model import Model
from sweet.model.objects import Objects


class TestObjects(unittest.TestCase):

    def test_init_from_model(self):
        class Demo(Model): pass

        objs = Demo.objects
        self.assertIsInstance(objs, Objects)
        self.assertIs(objs.model_class, Demo)


if __name__ == '__main__':
    unittest.main()
