# 1. Write a Python script to sort (ascending and descending) a dictionary by value. 

def add_keys(len, x_dict):
    
    for i in range(len):
        key = input('Enter key: ')
        val = input('Enter value: ')

        x_dict[key] = val
    
    return x_dict

def check_key(key, x_dict):
    x_dict[key] = x_dict.get(key, 0)

    return x_dict[key]

u_dict = {}

dict_len = int(input('Enter no of keys: '))

add_keys(dict_len, u_dict)

print('Original: ', u_dict)
print('\n')

u_list = sorted([(v, k) for k, v in u_dict.items()])
u_list_rev = sorted([(v, k) for k, v in u_dict.items()], reverse=True)

print(u_list)
print(u_list_rev)

# 2. Write a Python script to add a key to a dictionary.
x_len_question = input("Would you like to add to the dictionary? y/n: ")


if x_len_question.upper() == 'Y' or x_len_question.upper() == 'YES':
    while True:
        try:
            dict_x_len = int(input('How many keys would you like to add: '))
        except:
            print('Invalid input. Please enter an integer.')
            continue
        add_keys(dict_x_len, u_dict)
        break
    
    
else:
    exit(0)    

print('New: ', u_dict)
print('\n')

# Sample Dictionary : {0: 10, 1: 20}
# Expected Result : {0: 10, 1: 20, 2: 30}

# 3. Write a Python script to concatenate following dictionaries to create a new one.

# Sample Dictionary :
# dic1={1:10, 2:20}
# dic2={3:30, 4:40}
# dic3={5:50,6:60}
# Expected Result : {1: 10, 2: 20, 3: 30, 4: 40, 5: 50, 6: 60}

# 4. Write a Python script to check whether a given key already exists in a dictionary. 
while True:
    x_key = input('Which key would you like to check for: ')

    if check_key(x_key, u_dict) == 0:
        print('Key doesn\'t exist.')
        continue
    else:
        print('Key:', x_key , '\nValue:', check_key(x_key, u_dict))
        break

# 5. Write a Python program to iterate over dictionaries using for loops.
for k,v in u_dict.items():
    print(k,v)