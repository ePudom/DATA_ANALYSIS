# 5. Write a Python program to check the nth-1 string is a 
# proper substring of nth string in a given list of strings

str_list = []

while True:
    str_input = input('Enter string: ')
    
    if str_input == 'done':
        break
    
    str_list.append(str_input)
    continue

print(str_list)
print(str_list[-1])
print(str_list[-2])

if str_list[-2] in str_list[-1]:
    print('True')
else:
    print('False') 