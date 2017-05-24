import re
text = open('en_US.twitter.txt')
for line in text:
	if  re.search(r"A computer once beat me at chess, but it was no match for me at kickboxing", line ) != None:
		print line