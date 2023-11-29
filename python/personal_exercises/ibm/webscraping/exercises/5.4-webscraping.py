import requests
from bs4 import BeautifulSoup
import pandas as pd 

url = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-DA0321EN-SkillsNetwork/labs/datasets/HTMLColorCodes.html"

tables_ = pd.read_html(url)
print(tables_)