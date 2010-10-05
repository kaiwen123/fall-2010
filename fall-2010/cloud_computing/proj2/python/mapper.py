#!/usr/bin/evn python

import sys

for line in sys.stdin:
	line = line.strip().split("\t")
	key = line[0]
	words = line[1].split(" ")
	length = len(words)
	word = ""
	offset = -1
	for i in range(length): 
		offset += len(word)+1
		word = words[i].strip('.,;:\'\"')
		print word.lower(), key + ":" + str(offset)
