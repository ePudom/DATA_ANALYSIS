# pip3 install IPython
import requests
import os 
from PIL import Image
from IPython.display import IFrame

# # EXAMPLE

# url = 'https://www.ibm.com'
# response = requests.get(url)

# print(response.status_code)
# print(response.request.headers)
# print(response.headers)

# img_url = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/IDSNlogo.png'

# res = requests.get(img_url)

# print(res.headers['Content-Type'])

# path = os.path.join(os.getcwd(), 'image.png')

# with open(path, 'wb') as f:
#   f.write(res.content)

# Image.open(path)

# EXERCISE

# txt_url = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/labs/Module%205/data/Example1.txt'

# r = requests.get(txt_url)

# with open('file.txt', 'w') as f:
#   f.write(r.content.decode())

# GET REQUEST WITH URL PARAMETERS
url_get = 'http://httpbin.org/get'
params = {'name':'Joseph', 'ID':'123'}
response_get = requests.get(url_get, params=params)

# print(response_get.url)
# print(response_get.status_code)
# # print(response_get.text)
# print(response_get.headers['Content-Type'])
# print(response_get.json())

# POST REQUESTS 
url_post = 'http://httpbin.org/post'
response_post = requests.post(url_post, data=params)
print(response_post.url)
print(response_post.status_code)
# print(response_post.text)
print(response_post.headers['Content-Type'])
print(response_post.json())



