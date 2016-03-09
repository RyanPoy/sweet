#coding: utf8
import re


"""
Inflector for pluralize and singularize English nouns.
This is the default Inflector for the Inflector obj
"""
uncountable_words = [
    'equipment', 'information', 'rice', 'money', 
    'species', 'series', 'fish', 'sheep', 'sms'
]
pluralize_irregular_words = [
    ['person', 'people'],
    ['man', 'men'],
    ['child', 'children'],
    ['sex', 'sexes'],
    ['move', 'moves']
]
singularize_irregular_words = [ [ irregular[1], irregular[0] ] for irregular in pluralize_irregular_words]

pluralize_rules = [
    ['(?i)(quiz)$' , '\\1zes'],
    ['^(?i)(ox)$' , '\\1en'],
    ['(?i)([m|l])ouse$' , '\\1ice'],
    ['(?i)(matr|vert|ind)ix|ex$' , '\\1ices'],
    ['(?i)(x|ch|ss|sh)$' , '\\1es'],
    ['(?i)([^aeiouy]|qu)ies$' , '\\1y'],
    ['(?i)([^aeiouy]|qu)y$' , '\\1ies'],
    ['(?i)(hive)$' , '\\1s'],
    ['(?i)(?:([^f])fe|([lr])f)$' , '\\1\\2ves'],
    ['(?i)sis$' , 'ses'],
    ['(?i)([ti])um$' , '\\1a'],
    ['(?i)(buffal|tomat)o$' , '\\1oes'],
    ['(?i)(bu)s$' , '\\1ses'],
    ['(?i)(alias|status)' , '\\1es'],
    ['(?i)(octop|vir)us$' , '\\1i'],
    ['(?i)(ax|test)is$' , '\\1es'],
    ['(?i)s$' , 's'],
    ['(?i)$' , 's']
]
singularize_rules = [
    ['(?i)(quiz)zes$' , '\\1'],
    ['(?i)(matr)ices$' , '\\1ix'],
    ['(?i)(vert|ind)ices$' , '\\1ex'],
    ['(?i)^(ox)en' , '\\1'],
    ['(?i)(alias|status)es$' , '\\1'],
    ['(?i)([octop|vir])i$' , '\\1us'],
    ['(?i)(cris|ax|test)es$' , '\\1is'],
    ['(?i)(shoe)s$' , '\\1'],
    ['(?i)(o)es$' , '\\1'],
    ['(?i)(bus)es$' , '\\1'],
    ['(?i)([m|l])ice$' , '\\1ouse'],
    ['(?i)(x|ch|ss|sh)es$' , '\\1'],
    ['(?i)(m)ovies$' , '\\1ovie'],
    ['(?i)(s)eries$' , '\\1eries'],
    ['(?i)([^aeiouy]|qu)ies$' , '\\1y'],
    ['(?i)([lr])ves$' , '\\1f'],
    ['(?i)(tive)s$' , '\\1'],
    ['(?i)(hive)s$' , '\\1'],
    ['(?i)([^f])ves$' , '\\1fe'],
    ['(?i)(^analy)ses$' , '\\1sis'],
    ['(?i)((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$' , '\\1\\2sis'],
    ['(?i)([ti])a$' , '\\1um'],
    ['(?i)(n)ews$' , '\\1ews'],
    ['(?i)s$' , ''],
]
    
def singularize_of(word):
    '''Singularizes English nouns.'''
    return singularize_or_pluralize_of(word, singularize_rules, singularize_irregular_words)

def pluralize_of(word):
    '''Pluralizes English nouns.'''
    return singularize_or_pluralize_of(word, pluralize_rules, pluralize_irregular_words)
    
def singularize_or_pluralize_of(word, rules, irregular_words):
    def is_uncountable(word):
        for uncountable_word in uncountable_words:
            lower_cased_word = word.lower()
            if lower_cased_word.endswith(uncountable_word):
                return True
        return False
    
    def irregular(word, irregular_words):
        for irregular, dest in irregular_words:
            match = re.search('(' + irregular + ')$', word, re.IGNORECASE)
            if match:
                return re.sub('(?i)' + irregular + '$', match.expand('\\1')[0] + dest[1:], word)
        return None
    
    def core_deal(word, rules):
        for patten, replacement in rules:
            match = re.search(patten, word, re.IGNORECASE)
            if match :
                for k, group in enumerate(match.groups()):
                    if group == None :
                        replacement = replacement.replace('\\%s' % str(k + 1), '')
                return re.sub(patten, replacement, word)
        return word

    if is_uncountable(word):
        return word
    result = irregular(word, irregular_words)
    if result is not None:
        return result
    return core_deal(word, rules)


def java_of(name):
    """
    a_b_c       => ABC
    ab_ab_ab    => AbAbAb
    """
    def is_letter(ch):
        return 'a' <= ch <= 'z' or 'A' <= ch <= 'Z'
    
    def is_char(ch):
        return 'a' <= ch <= 'z' or 'A' <= ch <= 'Z' or '0' <= ch <= '9'
        
    name = python_of(name)
    pos = 0
    for i, ch in enumerate(name):
        if is_letter(ch):
            pos = i
            break
        
    java_name = []
    upper_flag = True
    for ch in name[pos:]:
        if upper_flag:
            java_name.append(ch.upper())
            upper_flag = False
        elif is_char(ch):
            java_name.append(ch)
        else:
            upper_flag = True
            
    return ''.join(java_name)

def python_of(name):
    """
    UserAddress  => user_address
    userAddress  => user_address
    User_Address => user_address
    user_Address => user_address
    User_address => user_address
    Useraddress  => useraddress
    """
    def append_under_line_to(python_name):
        if python_name[-1] != '_':
            python_name.append('_')
        
    python_name = [] # the first charactor
    for i, ch in enumerate(name):
        if i == 0: # the first charactor
            python_name.append(ch.lower())
        elif ch.isupper(): 
            append_under_line_to(python_name)
            python_name.append(ch.lower())
        elif ch == '_':
            append_under_line_to(python_name)
        else:
            python_name.append(ch.lower())
            
    return ''.join(python_name)

tableize_of = lambda name: pluralize_of(python_of(name))