# 4. We are making n stone piles! The first pile has n stones. 
# If n is even, then all piles have an even number of stones. 
# If n is odd, all piles have an odd number of stones. 
# Each pile must more stones than the previous pile but as few as possible. 
# Write a Python program to find the number of stones in each pile.

stone_piles = input('How many stone piles? ')

try:
    stone_piles = int(stone_piles)
except:
    print('Invalid. Please enter an integer.')

if stone_piles % 2 == 0:
    print('Even')
else: 
    print('Odd')

print([stone_piles + 2 * i for i in range(stone_piles)])


