from enum import Enum

class Logic(Enum):
    AND = 'AND'
    OR = 'OR'

    def __str__(self):
        return self.value

