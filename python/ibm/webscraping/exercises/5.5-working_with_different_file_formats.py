import pandas as pd 
import numpy as np
import json
import urllib.request
import xml.etree.ElementTree as ET
from PIL import Image
from IPython.display import display

########################################
##### WORKING WITH CSV FILE FORMAT #####
########################################

url ='https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/labs/Module%205/data/addresses.csv'
df = pd.read_csv(url, header=None)
# print(df)

df.columns = ['First Name', 'Last Name', 'Street Address', 'City', 'State', 'Area Code']
# print(df)

# Select one column
# print(df['First Name'])

# Select multiple columns
df_ = df[['First Name', 'City', 'State']]
# print(df_)

# Select the first row 
# print(df_.loc[0])

# Select  the first, second and third row of 'First Name' column
# print(df_.loc[[0,1,2], 'First Name']) # label-based selecting method


# print(df_.iloc[[0,1,2], 0]) # index-based selecting method

########################
## TRANSFORM FUNCTION ##
########################
df2 = pd.DataFrame(np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]]))
# print(df2)

# Say we want to add 10 to each element of the dataframe
df2_ = df2.transform(func=lambda x : x +10)
# print(df2_)

df2_sqrt = df2_.transform(func=['sqrt'])
# print(df2_sqrt)

#########################################
##### WORKING WITH JSON FILE FORMAT #####
#########################################

########################
# Write JSON to a File #
########################
person = {
  'first_name' : 'Mark',
  'last_name' : 'abc',
  'age' : 27,
  'address': {
    'streetAddress': '21 2nd Street',
    'city': 'New York',
    'state': 'NY',
    'postalCode': '10021-3100'
  }
}
# print(type(person))

#############################################################
## -- Serialization using json.dump(dict, file_pointer) -- ##
# used for writing to a JSON file 
#############################################################

# with open('../files/person.json', 'w') as f:
#   json.dump(person, f)

########################################################
## -- Serialization using json.dumps(dict, indent) -- ##
# used in converting a dictionary to a JSON object
########################################################

obj = json.dumps(person, indent=4)
# print(type(obj))

# with open('../files/sample.json', 'w') as outfile:
#   outfile.write(obj)


##########################
# Reading JSON to a File #
##########################

#####################################################
## -- Using json.load(file_pointer) -- ##
# loads the contents from a JSON file to a dictionary
#####################################################

with open('../files/sample.json', 'r') as openFile:
  obj_ = json.load(openFile)

# print(obj_)
# print(type(obj_))


#########################################
##### WORKING WITH XLSX FILE FORMAT #####
#########################################

###############################
# Reading from XLSX to a File #
###############################

# urllib.request.urlretrieve('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/labs/Module%205/data/file_example_XLSX_10.xlsx', '../files/sample.xlsx')

xlsx_df = pd.read_excel('../files/sample.xlsx')
# print(xlsx_df)


#########################################
##### WORKING WITH XML FILE FORMAT #####
#########################################

######################################################################################
# -- Writing with xml.etree.ElementTree -- ##
# xml.etree.ElementTree provides functionality for parsing and creating XML documents.
# ElementTree represents the XML documents as a tree.
# We can move accross the documents using nodes which are 
# elements and sub-elements of the XML file.
######################################################################################

# Create the file structure
employee = ET.Element('employee')
details = ET.SubElement(employee, 'details')
firstname = ET.SubElement(details, 'firstname')
lastname = ET.SubElement(details, 'lastname')
age = ET.SubElement(details, 'age')
firstname.text = 'Jane'
lastname.text = 'Smith'
age.text = '32'

# Create a new XML file with the data 
data = ET.ElementTree(employee)

# with open('../files/sample.xml', 'wb') as file:
#   data.write(file)

#############################################
# -- Reading with xml.etree.ElementTree -- ##
#############################################

# urllib.request.urlretrieve('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/labs/Module%205/data/Sample-employee-XML-file.xml', '../files/employee.xml')

tree = ET.parse('../files/employee.xml')
root = tree.getroot()
# print(root)

# columns = ['firstname','lastname','title','division','building','room']
rows = []

for node in root:
  firstname = node.find('firstname').text
  lastname = node.find('lastname').text
  title = node.find('title').text
  division = node.find('division').text
  building = node.find('building').text
  room = node.find('room').text

  rows.append({'firstname':firstname,'lastname':lastname,'title':title,'division':division,'building':building,'room':room})

tree_df = pd.DataFrame(rows)
print(tree_df)

#############################################
# -- Reading xml file pandas.read_xml() -- ##
#############################################

# We use xpath to mention the xml nodes to be considered
xml_df = pd.read_xml('../files/employee.xml', xpath='.//details') 
print(xml_df)


#####################
##### SAVE DATA #####
#####################

xml_df.to_csv('../files/employee.csv', index=False)

#######################################
# -- Read/Save Other Data Formats -- ##
#######################################

# | Data Formate  | Read           | Save             |
# | ------------- |:--------------:| ----------------:|
# | csv           | `pd.read_csv()`  |`df.to_csv()`     |
# | json          | `pd.read_json()` |`df.to_json()`    |
# | excel         | `pd.read_excel()`|`df.to_excel()`   |
# | hdf           | `pd.read_hdf()`  |`df.to_hdf()`     |
# | sql           | `pd.read_sql()`  |`df.to_sql()`     |
# | ...           |   ...          |       ...        |


##############################
##### BINARY FILE FORMAT #####
##############################

###################################################################
# -- Reading the Image file using PIL(Python Imaging Library) -- ##
###################################################################
urllib.request.urlretrieve('https://hips.hearstapps.com/hmg-prod.s3.amazonaws.com/images/dog-puppy-on-garden-royalty-free-image-1586966191.jpg', '../files/dog.jpg')

image = Image.open('../files/dog.jpg')

display(image)
