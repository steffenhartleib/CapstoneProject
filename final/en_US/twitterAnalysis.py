from __future__ import division
import re
love = 0
hate = 0
text = open('en_US.twitter.txt')

for line in text:
	if  re.search(r"\blove\b", line ) != None:
		love += 1
	if re.search(r"\bhate\b", line ) != None:
		hate += 1
ratio = float(love)/hate
print "Love",love
print "Hate", hate
print "Ratio", ratio




		
		
	
	