# 5. Write a Python program to count the number of strings 
# where the string length is 2 or more and the first and 
# last character are same from a given list of strings.
u_list = []

while True:
    u_input = input('Enter: ')

    if u_input == 'done':
        break
    else:
        u_list.append(u_input)

count = 0
for i in range(len(u_list)):
    if u_list[i][0] == u_list[i][-1]:
        count = count + 1

print('List: ', u_list)
print('Counted: ', count)