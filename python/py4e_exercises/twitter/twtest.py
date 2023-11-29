import urllib.request, urllib.parse, urllib.error
from twurl import augment
import ssl

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

print('Calling Twitter...')
url = augment('https://api.twitter.com/2/users/by/username/_z_bee', None)
print(url)
connection = urllib.request.urlopen(url, context=ctx)
data = connection.read()
print(data)
headers = dict(connection.getheaders())
print(headers)
