name = input("Enter file:")
# if len(name) < 1:
#     name = "mbox-short.txt"
handle = open(name)

dict_x = {}
words = []
for line in handle:
    if not line.startswith('From '):
        continue
    else:
        line = line.split()[1]
        words.append(line)

for word in words:
    dict_x[word] = dict_x.get(word, 0) + 1

# print(dict_x)
max_count = None
max_word = None

for word, count in dict_x.items():
    if max_count is None or count > max_count:
        max_count = count 
        max_word = word 

print(max_word, max_count)
