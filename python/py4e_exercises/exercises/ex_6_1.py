text = "X-DSPAM-Confidence:    0.8475"

x = text.find('0')
sliced = float(text[x:])

print(sliced)
