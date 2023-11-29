# 2. Write a Python program that accept a list of integers and 
# check the length and the fifth element. Return true if the 
# length of the list is 8 and fifth element occurs thrice in the said list.


u_list = []

while True:
    u_input = input('Enter integer: ')
    
    if u_input == 'done':
        break
    else:
        try:
            int(u_input)
        except:
            print('Please enter an integer.')

        u_list.append(u_input)
        continue

if len(u_list) == 8 and u_list.count(u_list[4]) == 3:
    print('True.\nLength of list: {0}.\nFifth element: {1}\nOccurence: {2}'.format(len(u_list), u_list[4], u_list.count(u_list[4])))
elif len(u_list) != 8:
    print('False.\nLength of Length of list: {0} (should be 8)'.format(len(u_list)))
elif u_list.count(u_list[4]) != 3:
    print('False.\nFifth element: {0}\nOccurence:{1} (should be 3)'.format( u_list[4], u_list.count(u_list[4])))
else:
    print('Invalid result/parameters.\nPlease try again.')



    



