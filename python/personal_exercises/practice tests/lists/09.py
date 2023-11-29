u_list = []

while True:
    u_input = input('Enter list: ')

    if u_input == 'done':
        break
    else:
        u_list.append(u_input)

print('Original list', u_list)

# 8. Write a Python program to check a list is empty or not.

if not u_list:
    print('List is empty')
    quit() 

# 7. Write a Python program to remove duplicates from a list.

uniq_list = []

for i in u_list:
    if i not in uniq_list:
        uniq_list.append(i)

print('Unique list', uniq_list)

# 9. Write a Python program to clone or copy a list.

u_list_copy = u_list.copy()

print('Copied list', u_list_copy)

# 10. Write a Python program to find the list of words that are longer than n from a given list of words.

REQ = 5

print('The following words have more than 5 letters...')
for i in u_list:
    if len(i) > 5:
        print(i, len(i))
