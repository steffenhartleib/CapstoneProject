# sample = open('cleanSample5000.txt','r')
# sampleData = sample.read()

import re
import pandas as pd
from pandas import Series
#rareWords = open('/Users/steffenhartleib/Google_Drive/Capstone/data/rareWords.csv')
rareWords = pd.read_csv('/Users/steffenhartleib/Google_Drive/Capstone/data/rareWords.csv')
rareWords = rareWords.terms
sample = open('/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmall/cleanSample5000.txt','r')
sampleData = sample.read()

test = sampleData
rare = rareWords

for w in rare:
	wb = "\\b" + w + "\\b"
	wr = re.compile(wb)
	test  = wr.sub("<unk>",test)
print test
	

#w = "out"     # word to be replaced
#wb = "\\b" + w + "\\b"
#wr = re.compile(wb)   #  word regex object to be replaced
#testR  = wr.sub("<unk>",test)      #function to replace wr with "<unk>" in the test file
#print testR
 
	
 