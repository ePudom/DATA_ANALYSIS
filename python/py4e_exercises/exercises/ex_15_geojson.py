import urllib.request, urllib.parse, urllib.error
import json
import ssl

ques = input('Do you have an API_KEY?(Y/N) ')

if (ques == 'Y' or ques.lower() == 'y'):
  api_key = input("Enter API_KEY: ")
  serviceurl = 'https://maps.googleapis.com/maps/api/geocode/json?'
elif (ques == 'N' or ques.lower() == 'n'):
  api_key = 42
  serviceurl = 'http://py4e-data.dr-chuck.net/json?'
else: 
  print('Wrong input')

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

while True:
  address = input('Enter location: ')
  if len(address) < 1: break

  params = dict()
  params['address'] = address
  if api_key != "": params['key'] = api_key

  url_x = serviceurl + urllib.parse.urlencode(params)
  print('Retrieving ', url_x)

  url = urllib.request.urlopen(url_x, context=ctx)
  data = url.read().decode()
  print('Retrieved ', len(data), ' characters')
  # print(data)

  try:
    js = json.loads(data)
  except:
    js = None
    

  if not js or 'status' not in js or js['status'] != 'OK':
    print("=====Failure retrieving data=====")
    print(data)
    continue

  print(json.dumps(js, indent=4))

  lat = js['results'][0]['geometry']['location']['lat']
  lng = js['results'][0]['geometry']['location']['lng']
  location = js['results'][0]['formatted_address']
  print('Latitude: ', lat)
  print('Longitude: ', lng)
  print('Location: ', location)
  
  

