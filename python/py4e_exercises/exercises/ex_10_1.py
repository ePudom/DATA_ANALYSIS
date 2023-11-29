# 10.2 Write a program to read through the mbox-short.txt and figure out 
# the distribution by hour of the day for each of the messages. 
# You can pull the hour out from the 'From ' line by finding the time 
# and then splitting the string a second time using a colon.
# ----From stephen.marquard@uct.ac.za Sat Jan  5 09:14:16 2008----
# Once you have accumulated the counts for each hour, 
# print out the counts, sorted by hour as shown below.

name = input("Enter file:")
# if len(name) < 1:
#     name = "mbox-short.txt"
handle = open(name)

dict_x = {}
words =[]

# Read line in the file and get the needed number
for line in handle:
    if not line.startswith('From '):
        continue
    else:
        line = line.split()[5].split(':')[0]
        words.append(line)

# Convert list to dictionary
for word in words:
    dict_x[word] = dict_x.get(word, 0) + 1

# Sort dictionary as list of tuples
new_list = sorted([(k, v) for k, v in dict_x.items()])

# Print list
for x, y in new_list:
    print(x, y)