# This script is to get the link citation of the input xml file. 
# Sample output will be like: 
# <rfc:anchorcite lni="3RRT-8GR0-003G-105M-00000-00" rfctokenref="I4PXW9SS0K1MNJ28P0000400" status="valid">
# </rfc:anchorcite>
egrep -o '<\/?rfc:anchorcite[^>]*>' input_1.xml | less
