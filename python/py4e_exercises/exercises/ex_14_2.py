import urllib.request, urllib.parse, urllib.error
import xml.etree.ElementTree as ET
import ssl

# Ignore SSL certificate 
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# Retrieve URL data
url = input('Enter URL: ')
data = urllib.request.urlopen(url, context=ctx).read()
print('Retrieving ', url)
print('Retieved ', len(data), 'characters')

# Parse XML string
tree = ET.fromstring(data)
comments = tree.findall('.//comment')
print('Count: ', len(comments))

total = 0

# Sum all comment count
for comment in comments:
  num = comment.find('count').text
  total += int(num)
  
print('Sum: ', total)



# http://py4e-data.dr-chuck.net/comments_42.xml