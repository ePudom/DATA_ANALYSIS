import urllib.request, urllib.parse, urllib.error
import ssl
from bs4 import BeautifulSoup

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

url = input('Enter URL: ')

def check_int(value):
  while True:
    try: 
      value = int(value)
      break
    except:
      print('Should be an integer.')
      quit()

  return value

count = check_int(input('Enter count: '))
position = check_int(input('Enter position: '))


# while count >= 0:
html = urllib.request.urlopen(url, context=ctx)
print(html)
test = html.read()
  # soup = BeautifulSoup(html, 'html.parser')
  # tags = soup('a')
  # # print(url)
  # url = tags[position - 1].get('href', None) 
  # count -= 1
  

