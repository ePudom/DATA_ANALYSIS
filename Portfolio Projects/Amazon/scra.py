from bs4 import BeautifulSoup
import requests
import smtplib
import time
import datetime
import json
import sqlite3




url = 'https://www.amazon.com/HP-Pavilion-Business-i7-1065G7-Bluetooth/dp/B0CKG375NQ/ref=sr_1_1_sspa?adgrpid=83201338004&hvadid=585412556860&hvdev=c&hvlocphy=1010294&hvnetw=g&hvqmt=e&hvrand=1908351041153966407&hvtargid=kwd-316007319283&hydadcr=21191_13331828&keywords=amazon%2Bhp%2Blaptop&qid=1700653358&sr=8-1-spons&sp_csd=d2lkZ2V0TmFtZT1zcF9hdGY&th=1'
headers = {
  "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7", 
  "Accept-Encoding": "gzip, deflate", 
  "Accept-Language": "en-US,en;q=0.9", 
  "Host": "httpbin.org", 
  "Upgrade-Insecure-Requests": "1", 
  "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36", 
  "X-Amzn-Trace-Id": "Root=1-655de7e0-7e93f7333962422a75fc0ba3"
}

response = requests.get(url, headers)

soup = BeautifulSoup(BeautifulSoup(response.content, 'html.parser').prettify(), 'html.parser')

product_title = soup.find(id = 'productTitle').get_text().strip()

product_price = soup.find(class_ = 'a-offscreen').get_text().strip()[1:]

avg_rating = soup.find(id = 'averageCustomerReviews').find(class_ = 'a-size-base').get_text().strip()

no_of_ratings = soup.find(id = 'acrCustomerReviewText').get_text().strip().split()[0]

product_details_li = soup.find(id = 'feature-bullets').find_all('li')

seller = soup.find(id = 'sellerProfileTriggerId').get_text().strip()

seller_certification = soup.find(id = 'sellerCertificationsODF_feature_div').find('div').get_text().split(':')[1].strip()

product_details = ""
count = 0

for i, span_tag in enumerate(product_details_li):
  count = count + 1
  detail_list = span_tag.find_all('span')

  for j, text in enumerate(detail_list):
    detail = str(text.get_text()).strip()
    product_details = product_details + ' (' + str(count) + '.) ' + detail + ' '

print(product_title)
print(product_price)
print(avg_rating)
print(no_of_ratings)
print(seller)
print(seller_certification)
print(product_details)

####################
## INSERT INTO DB ##
####################
conn = sqlite3.connect('db/amazon.sqlite')
curr = conn.cursor()

curr.executescript('''
  CREATE TABLE IF NOT EXISTS Sellers (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    name TEXT NOT NULL UNIQUE,
    certification TEXT
  );
             
  CREATE TABLE IF NOT EXISTS Products (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    title TEXT NOT NULL,
    price INTEGER,
    average_rating FLOAT,
    no_of_ratings INTEGER,
    seller_id INTEGER,
    CONSTRAINT fk_sellers FOREIGN KEY (seller_id) REFERENCES Sellers (id)
  );
''')

curr.execute('''
  INSERT OR IGNORE INTO Sellers (name, certification) VALUES (?,?)
''', (seller, seller_certification))

curr.execute('''
  SELECT id FROM Sellers WHERE name = ?
''', (seller,))
seller_id = curr.fetchone()[0]

curr.execute('''
  INSERT OR REPLACE INTO Products (title, price, average_rating, no_of_ratings, seller_id) 
  VALUES (?, ?, ?, ?, ?)
''', (product_title, product_price, avg_rating, no_of_ratings, seller_id))

conn.commit()
