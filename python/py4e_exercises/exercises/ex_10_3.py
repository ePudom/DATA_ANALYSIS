# Write a program that reads a file and prints the letters
# in decreasing order of frequency. Your program should convert all the
# input to lower case and only count the letters a-z. Your program should
# not count spaces, digits, punctuation, or anything other than the letters
# a-z. Find text samples from several different languages and see how
# letter frequency varies between languages. Compare your results with
# the tables at https://wikipedia.org/wiki/Letter_frequencies.


u_file = input('Enter file: ')

handle = open(u_file)

dict_x = {}

letters = []

for line in handle:
    words = line.strip().split()

    # print(words)

    for word in words:
        for i in range(len(word)):
            letters.append(word[i].lower())

for letter in letters:
    dict_x[letter] = dict_x.get(letter, 0) + 1

total_v = 0
for k, v in dict_x.items():
    v = list()

letters = sorted([(k, v) for k,v in dict_x.items()])
print(letters)




# for k, v in letters:
#     total_val = sum(v)
#     per_v = (float(v)/float(total_val)) * 100
#     print(k, per_v)

