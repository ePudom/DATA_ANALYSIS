import urllib.request, urllib.parse, urllib.error
import json
import ssl 

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

url = input('Enter URL: ')

connection = urllib.request.urlopen(url, context=ctx)
print('Retrieving ', url)
data = connection.read().decode()
print('Retrieved ', len(data), ' characters.')

try:
  js = json.loads(data)
except:
  js = None

total = 0
count = 0

for i in js['comments']:
  count += 1
  total += int(i['count'])

print('Count: ', count)
print('Sum: ', total)


# http://py4e-data.dr-chuck.net/comments_42.json
