# Use words.txt as the file name
fname = input("Enter file name: ")
try:
    fh = open(fname)
except:
    print('Invalid file')
    quit()

for line in fh:
    line = line.upper().rstrip()
    print(line)