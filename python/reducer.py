import sys
from operator import itemgetter

wordindex = {}
default = ""
for line in sys.stdin:
	word, index = line.split(" ")
	count = 0
	if(not wordindex.has_key(word)):
		wordindex[word] = wordindex.get(word, default) + index.strip()
	else:
		wordindex[word] += " " + index.strip()

for key in wordindex.keys():
	print key + "\t" + wordindex[key]
