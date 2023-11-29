# Unpacking variables
# 1
# p = 1,3,4,5,6,3
# a,b,c,d,e,f = p

# print(a)

# 2
# data = ('Alan', 'Barry', 'White', '5, Alley Avenue', 'NY', '09022334455', '08122334455')
# fname, middlename, lname, street, country, *phonenum = data 

# print(phonenum)

# 3
# dataset = (23.5,22,45.6,20,)
# *nums, num = dataset

# nums_avg = sum(nums)/len(nums)
# print(nums_avg, num)

line = 'nobody:*:-2:-2:Unprivileged User:/var/empty:/usr/bin/false'
uname, *fields, homedir, sh = line.split(':')

# print(homedir)
word = 'Hello'
type(word)
dir(word)
word.s