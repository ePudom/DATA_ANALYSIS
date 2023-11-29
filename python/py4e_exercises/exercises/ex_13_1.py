import urllib.request, urllib.parse, urllib.error
from bs4 import BeautifulSoup
import ssl

# Ignore SSL certificate 
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# Enter URL and read file 
url = input('Enter URL: ')
html = urllib.request.urlopen(url, context=ctx).read()
soup = BeautifulSoup(html, 'html.parser')

tags = soup('span')

count = 0
total = 0

# Count and sum up numbers in the 'span' tag 
for tag in tags:
  count += 1
  total = total + int(tag.contents[0])

# Print result 
print('Count: ', count)
print('Sum: ', total)



