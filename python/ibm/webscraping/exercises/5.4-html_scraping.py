from bs4 import BeautifulSoup

html='''
  <!DOCTYPE html>
  <html>
    <head>
      <title>Page Title</title>
    </head>
    <body>
      <h3><b id='boldest'>Lebron James</b></h3>
      <p> Salary: $ 92,000,000 </p>
      <h3> Stephen Curry</h3>
      <p> Salary: $85,000, 000 </p>
      <h3> Kevin Durant </h3>
      <p> Salary: $73,200, 000</p>
    </body>
  </html>
'''

soup = BeautifulSoup(html, 'html5lib')

# print(soup.prettify())

tag_title = soup.title
# print("title: ", tag_title)
# print(type(tag_title)) # <class 'bs4.element.Tag'>

tag_h3 = soup.h3
# print("h3: ", tag_h3)

tag_h3_child = tag_h3.b
# print("h3 child(b): ", tag_h3_child)
# print("h3 child(b) attr(id): ", tag_h3_child['id'])
# print("h3 child(b) parent: ", tag_h3_child.parent)

tag_h3_sibling_1 = tag_h3.next_sibling.next_sibling
# print("h3 sibling 1: ", tag_h3_sibling_1)
# print(type(tag_h3_sibling_1))

tag_h3_sibling_2 = tag_h3_sibling_1.next_sibling.next_sibling
# print("h3 sibling 2: ", tag_h3_sibling_2)

tag_h3_sibling_3 = tag_h3_sibling_2.next_sibling.next_sibling
# print("h3 sibling 3: ", tag_h3_sibling_3)

tag_h3_siblings = tag_h3.find_next_siblings()
# print("h3 siblings: ", tag_h3_siblings)

tag_h3_string = tag_h3.string
tag_h3_name = tag_h3.name
# print(tag_h3_string, tag_h3_name)

table = '''
  <table>
    <tr>
      <td id='flight' >Flight No</td>
      <td>Launch site</td> 
      <td>Payload mass</td>
    </tr>
    <tr> 
      <td>1</td>
      <td><a href='https://en.wikipedia.org/wiki/Florida'>Florida</a></td>
      <td>300 kg</td>
    </tr>
    <tr>
      <td>2</td>
      <td><a href='https://en.wikipedia.org/wiki/Texas'>Texas</a></td>
      <td>94 kg</td>
    </tr>
    <tr>
      <td>3</td>
      <td><a href='https://en.wikipedia.org/wiki/Florida'>Florida<a> </td>
      <td>80 kg</td>
    </tr>
  </table>
'''

table_soup  = BeautifulSoup(table, 'html5lib')

# tb_tag_a = table_soup.find_all('a')
# print(tb_tag_a)

# Retrieve links in anchor tags
# for link in tb_tag_a:
#   # To retrieve the 'href' value, use this  
#   print(link.get('href')) # This returns None if the attribute does not exist
#   # or 
#   try:
#     print(link['href']) # This errors out if the attribute does not exist, hence the try-except block
#   except:
#     print('tag not found')

# Retrieve table cells 
tb_rows = table_soup.find_all('tr')

# # print(tb_rows)

# # for row in tb_rows:
# #   print(type(row))

for i, row in enumerate(tb_rows):
  print("row", i)

  tb_cells = row.find_all('td')

  for j, cell in enumerate(tb_cells):
    print("column", j)
    print("cell", str(cell.string))

# Pass a list in the find_all method 
list_tags = table_soup.find_all(name=['tr', 'td'])
# print(list_tags)

tb_rows_id = table_soup.find_all(id="flight")
# print(tb_rows_id)

tb_rows_href = table_soup.find_all(href=True)
# print(tb_rows_href)

soup_id = soup.find_all(id='boldest')
# print(soup_id)

doc='''
  <h3>Rocket Launch </h3>
  <p>
    <table class='rocket'>
      <tr>
        <td>Flight No</td>
        <td>Launch site</td> 
        <td>Payload mass</td>
      </tr>
      <tr>
        <td>1</td>
        <td>Florida</td>
        <td>300 kg</td>
      </tr>
      <tr>
        <td>2</td>
        <td>Texas</td>
        <td>94 kg</td>
      </tr>
      <tr>
        <td>3</td>
        <td>Florida </td>
        <td>80 kg</td>
      </tr>
    </table>
  </p>
  
  <h3>Pizza Party  </h3>
  <p>
    <table class='pizza'>
      <tr>
        <td>Pizza Place</td>
        <td>Orders</td> 
        <td>Slices </td>
      </tr>
      <tr>
        <td>Domino's Pizza</td>
        <td>10</td>
        <td>100</td>
      </tr>
      <tr>
        <td>Little Caesars</td>
        <td>12</td>
        <td >144 </td>
      </tr>
      <tr>
        <td>Papa John's </td>
        <td>15 </td>
        <td>165</td>
      </tr>
    </table>
  </p>
'''

doc_soup = BeautifulSoup(doc, 'html5lib')

doc_tb_1 = doc_soup.find('table')
print(doc_tb_1)

doc_tb_2 = doc_soup.find('table', class_='pizza')
print(doc_tb_2)

 