import csv
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import urllib.parse

API_KEY = "f3a2132b-8986-4975-b0d8-47d1c186e0dc"

base_url = 'https://ng.indeed.com'

def get_url(job_title, location):
    """Generate URL based on the job title and location"""
    template = base_url + '/jobs?q={}&l={}'
    url = template.format(job_title, location)
    return url

def get_scrapeops_url(url):
    payload = {'api_key': API_KEY, 'url': url}
    proxy_url = 'https://proxy.scrapeops.io/v1/?' + urllib.parse.urlencode(payload)
    return proxy_url

url = get_scrapeops_url(get_url("data analyst", "lagos"))

response = requests.get(url)

soup = BeautifulSoup(response.text, 'html.parser')

cards = soup.find_all(class_ = 'job_seen_beacon')

def extract_data(card):
    """Extact job record from a single record"""
    header_content = card.find('table', 'jobCard_mainContent')

    atag = header_content.h2.a
    job_title = atag.span.text

    job_url = base_url + atag.get('href')
    
    company_ = header_content.find('div', 'company_location').span
    company = company_.text

    company_location = company_.next_sibling.text

    metadata = header_content.find('div', 'metadataContainer').find_all('div', 'metadata')
    salary = ""
    time_commitment = ""

    if len(metadata) > 1:
        salary = metadata[0].div.text
        time_commitment = metadata[1].div.text
    else:
        time_commitment = metadata[0].div.text

    body_content = header_content.nextSibling

    job_description_ = body_content.find('div', 'job-snippet').find_all('li')
    job_description = ""

    for description in job_description_:
        job_description += description.text + "\n"

    post_date_ = body_content.find('span', 'date')

    unwanted = post_date_.find('span', 'visually-hidden')
    unwanted.extract()

    post_date = post_date_.text

    today = datetime.today().strftime("%Y-%m-%d")

    record = (job_title, job_url, company, company_location, salary, time_commitment, job_description, post_date, today)

    return record

records = []

for card in cards:
    record = extract_data(card)
    records.append(record)