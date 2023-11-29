# Ask user for no of int 
# If user input is not a number return error till user enters int 
# Ask user to enter the in based on the no earlier provided 
# If input is not an int return 

# Verify integer
def is_verified_int(x):

        try:
            int(x)
            return True
        except:
            print("Input must be an integer")
            return False

# While program is running
while True: 
    user_int = input("How many integers: ")

    # if user input is an integer 
    if is_verified_int(user_int) == True:
        
        # initialize integer list and counter
        user_int_vals = list()
        count = 0

        # repeat for length of user input
        while count < int(user_int):
            # user enters integer value 
            int_val = input("Enter integer {0}: ".format(count + 1))

            # if value is an integer 
            if is_verified_int(int_val):
                # add to integer list 
                user_int_vals.append(int_val)
                # update counter 
                count += 1

        # print user integer value 
        for i in user_int_vals:
            print(str(i))
            
        break
    # if user input is not an integer, go back to beginning of 'while' loop 
    else: 
        continue
