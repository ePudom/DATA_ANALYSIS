u_list = []

while True:
    u_input = input('Enter: ')

    if u_input == 'done':
        break
    else:
        try: 
            u_input = int(u_input)
        except:
            print('Invalid')
        u_list.append(u_input)
# 1. Write a Python program to sum all the items in a list.
print('List: ', u_list)
print('Sum: ', sum(u_list))

# 2. Write a Python program to multiply all the items in a list.
mul = 1
for i in range(len(u_list)):
    mul = mul * u_list[i]
print('Multipication: ', mul)

# 3. Write a Python program to get the largest number from a list.
print('Largest: ', max(u_list))

# 4. Write a Python program to get the smallest number from a list.
print('Smallest: ', min(u_list))