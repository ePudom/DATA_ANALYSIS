# 1. Write a Python program find a list of integers with exactly two 
# occurrences of nineteen and at least three occurrences of five.

u_list = [19,22,34,53,12,19,2,3,4,5,5,5,1]

if u_list.count(19) == 2 and u_list.count(5) >= 3:
    print('Initial list: ', u_list)
    print('True. There are {0} 19s and {1} 5s.'.format(str(u_list.count(19)), str(u_list.count(5))))
else:
    print('False. There are {0} 19s and {1} 5s. Please try again.'.format(str(u_list.count(19)), str(u_list.count(5))))
    