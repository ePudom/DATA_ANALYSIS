# 3. Write a Python program that accept an integer test whether 
# an integer greater than 4^4 and which is 4 mod 34.
CHECKER = 4**4

def check_num(num):
    if num > CHECKER:
        if num % 34 == 4:
            return True 
        else: 
            return False
    else:
        return False

u_input = input('Enter integer: ')

try:
    print(check_num(int(u_input)))
except:
    print('Input should be an integer. Please try again.')

