# 6. Write a Python program to get a list, sorted in increasing order by the 
# last element in each tuple from a given list of non-empty tuples.

# Function verifies that input is an integer
def isIntVerified(num):
    try:
        int(num)
        return True 
    except:
        return False

# Function returns the last element in a list or tuple 
def last(n): return n[-1]

# Function returns a sorted list based on the last digit in a tuple
def sort_tuple_list(x_list):
    x_list = sorted(x_list, key=last)

    return x_list

print('This program sorts, in ascending order, \na list of tuples based on the last item in the tuple.\n')
# Initialize overall list 
u_list = []

# Ask user for no of items in the list 
list_items = input('How many list items: ')

# Verify no of list items is an integer 
if isIntVerified(list_items) is True:

    # Iterate throught list items 
    for i in range(int(list_items)):

        print('\nEnter tuple ', i + 1, '\n')

        # For each list item, ask user for no of items in a tuple
        tuple_items = int(input('How many tuple items: '))

        # Verify no of tuple items is an integer 
        if isIntVerified(tuple_items) is True:

            # Initialize tuple items as list 
            u_tuple_items = []

            # Iterate through tuple items 
            for j in range(tuple_items):

                # Ask user to enter tuple items 
                tuple_item = input('Enter: ')

                # Add each tuple item to the tuple items list 
                u_tuple_items.append(tuple_item)
                # print(u_tuple_items)

            # Convert list to tuple and add to overall list i.e u_list
            u_list.append(tuple(u_tuple_items))

# Print out original list 
print('\nOriginal list: ', u_list)

# Print out sorted list 
print('\nSorted list: ', sort_tuple_list(u_list))