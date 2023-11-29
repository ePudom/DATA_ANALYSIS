import urllib.request, urllib.parse, urllib.error
import xml.etree.ElementTree as ET
import ssl 

# Get API_KEY 
ques = input('Do you have an API_KEY?(Y/N) ')

if (ques == 'Y' or ques.lower() == 'y'):
  api_key = input("Enter API_KEY: ")
  serviceurl = 'https://maps.googleapis.com/maps/api/geocode/xml?'
elif (ques == 'N' or ques.lower() == 'n'):
  api_key = 42
  serviceurl = 'http://py4e-data.dr-chuck.net/xml?'
else: 
  print('Wrong input')
  
# Ignore SSL certificate
ctx = ssl.create_default_context()
ctx.check_hostname = False 
ctx.verify_mode = ssl.CERT_NONE

while True:
  address = input('Enter location: ')
  if len(address) < 1: break

  params = dict()
  params['address'] = address
  if (api_key != ''): params['key'] = api_key

  url_x = serviceurl + urllib.parse.urlencode(params)
  print('Retrieving ', url_x)

  data = urllib.request.urlopen(url_x, context=ctx).read()
  print('Retrieved ', len(data), ' characters.')
  print(data.decode())

  tree = ET.fromstring(data)

  results = tree.findall('result')
  lat = results[0].find('geometry').find('location').find('lat').text
  lng = results[0].find('geometry').find('location').find('lng').text
  location = results[0].find('formatted_address').text

  print('Longitude: ', lng)
  print('Latitude: ', lat)
  print('Location: ', location)



