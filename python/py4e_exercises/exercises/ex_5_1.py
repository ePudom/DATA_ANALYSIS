largest = None
smallest = None

while True:
    num = input("Enter a number: ")

    if num == "done":
        break
        
    try:
        fnum = int(num)
    except: 
        print('Invalid input')
        continue
    
    if smallest is None:
        smallest = fnum
    elif smallest > fnum:
        smallest = fnum

    if largest is None:
        largest = fnum
    elif largest < fnum:
        largest = fnum

print('Mininum is equal to', smallest)    
print('Maximum is equal to', largest)
