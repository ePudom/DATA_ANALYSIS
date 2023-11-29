# Use the file name mbox-short.txt as the file name
def average(l):
    t = 0

    for i in l:
        t += i
    
    a = t/len(l)

    return a


fname = input("Enter file name: ")
fh = open(fname)
line_list = []
for line in fh:
    if not line.startswith("X-DSPAM-Confidence:"):
        continue
    else:
        line = line[(line.find(':') + 1):].strip()
        
    try:
        line_list.append(float(line))
    except:
        continue


print(average(line_list))

print("Done")
