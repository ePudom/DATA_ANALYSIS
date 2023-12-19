class TextAnalyzer(object):
    
  def __init__(self, text):
    formatted_text = text.replace('!', '').replace('.','').replace(',', '')
    formatted_text = formatted_text.lower()

    self.frmt_text = formatted_text

  def freqAll(self):
    wordList = self.frmt_text.split(' ')
    wordDict = {}

    for word in set(wordList):
      wordDict[word] = wordList.count(word)
    
    return wordDict
  
  def freqWord(self, word):
    freqMap = self.freqAll()

    if word in freqMap:
      return freqMap[word]
    else:
      return 0
    

given_text = input("Enter text: ")

analyzed_text = TextAnalyzer(given_text)
print("\nFormatted text: \n", analyzed_text.frmt_text, "\n")

print("Text frequency map is as follows: \n", analyzed_text.freqAll(), "\n")
search_word = input("Enter search word: ")

print("Frequency of ", search_word, " is", analyzed_text.freqWord(search_word))
