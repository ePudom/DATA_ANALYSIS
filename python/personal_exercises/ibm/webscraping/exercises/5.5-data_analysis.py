import pandas as pd 
import urllib.request
import matplotlib.pyplot as plt
import seaborn as sns

csv = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0101EN-SkillsNetwork/labs/Module%205/data/diabetes.csv'

# urllib.request.urlretrieve(csv, '../files/diabetes_dataset.csv')

df = pd.read_csv('../files/diabetes_dataset.csv')

print('First 5 rows: ')
print(df.head(5)) 

##########################
##### DATA STRUCTURE #####
##########################

################################################
## -- shape(): Dimensions of the dataframe -- ##
################################################

print('Dimensions: ', df.shape) 

########################################################################
## -- info(): This method prints information about a DataFrame including
#  the index dtype and columns, non-null values and memory usage -- ##
########################################################################

print('Information: ', df.info())

#####################################################
## -- describe(): Used to view some basic statistical 
# details about the dataframe -- ##
#####################################################

print('Statistical Overview: ', df.describe())

##########################
##### MISSING VALUES #####
##########################

#####################################################
## -- isnull() or isna(): Used to check for missing values -- ##
## -- Count the number of unique values in each column, 
# in this case True and False -- ##
#####################################################

missing_data = df.isna()
print(missing_data.head(5))

for col in missing_data.columns:
  # print(col)
  print(missing_data[col].value_counts())
  print('')

#######################
##### DATA FORMAT #####
#######################

##############################################################
## -- dtypes: Used to check the data type of all columns
## -- dtype(): Used to check the data type of a column -- ##
## -- astype(): Used to change the data type of a column -- ##
##############################################################

print(df.dtypes)

#########################
##### VISUALIZATION #####
#########################

#####################################################################################
## -- Seaborn and matplotlib are Python's most powerful visualization libraries -- ##
#####################################################################################

labels = 'Diabetic', 'Not Diabetic'
plt.pie(df['Outcome'].value_counts(), labels=labels, autopct='%0.02f%%')
plt.legend()
plt.show()

