import re 

u_file = input('Enter file name: ')

# Open file
handle = open(u_file)
u_list = []

for line in handle:
    line = line.strip()

    # Find all numbers in the per line 
    num_list = re.findall('[0-9]+', line)

    # if no number is found, continue 
    if len(num_list) < 1: continue

    for num in num_list:
        # Append number to the list 
        u_list.append(int(num))

# Print out the length and sum of the numbers
print('Length: ', len(u_list))
print('Sum: ', sum(u_list))