import sys
def tobinary(filename):
    ''' This program transforms the profile output to binary format. '''
    pfile = open(filename, 'r')
    for line in pfile: 
        items = line.split('\t')
        for item in items:
            print item, ' ' 

            continue 

            item_pair = item.split(':')
            if len(item_pair) == 1:
                x = 1
                #print item_pair[0]
            elif len(item_pair) == 2:
                #print item_pair[0], '-', 
                # print item_pair[1]
                value = str(item_pair[1])
                if value.find('N/A') >= 0:
                    print '0',
                elif value.find('NO') >= 0:
                    print '0',
                else: 
                    print '1',
        print

if __name__ == "__main__":
    tobinary(sys.argv[1])
