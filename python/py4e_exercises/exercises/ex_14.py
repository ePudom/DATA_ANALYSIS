import xml.etree.ElementTree as ET

x = '''<person>
  <name>Bisola</name>
  <age>22</age>
  <email hide="Yes" />
</person>'''

y = '''<stuff>
  <users>
    <user x="1">
      <id>001</id>
      <name>Bryan</name>
      <email hide="Yes" />
    </user>
    <user x="2">
      <id>003</id>
      <name>Kate</name>
      <email hide="No" />
    </user>
  </users>
</stuff>'''

def test(inp):
  try :
    tree = ET.fromstring(inp)
  except:
    print('Cannot parse string to XML')

  return tree

def func_x(x):
  new_tree = test(x)

  print('Name: ', new_tree.find('name').text)
  print('Age: ', new_tree.find('age').text)
  print('Attr: ', new_tree.find('email').get('hide'))

def func_y(y):
  new_tree = test(y)
  new_list = new_tree.findall('users/user')

  for tree in new_list:
    print('ID: ', tree.find('id').text)
    print('Name: ', tree.find('name').text)
    print('Attr: ', tree.find('email').get('hide'))
    print('\n')


data = input("Choose 1(data1) or 2(data2): ")

if (data == '1') : 
  data = x
  func_x(data)
elif (data == '2'): 
  data = y
  func_y(data)
else: print('Wrong input.')




