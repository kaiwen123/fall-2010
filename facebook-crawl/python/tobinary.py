import sys
def tobinary(filename, idd):
    ''' This program transforms the profile output to binary format. '''
    pfile = open(filename, 'r')
    DEBUG_FORMAT1 = False
    DEBUG_FORMAT = False
    for line in pfile: 
        items = line.split('\t')

        if len(items) <> 15: continue
        if idd <> 'id': result = ''
        else: result = items[0] + ','

        #print len(items), items[0]

        # basic information. 
        item = items[12]
        # sex
        if item.find('Sex') >= 0: result += 'Y,'
        else: result += 'N,'
        # networks
        if item.find('Networks') >= 0: result += 'Y,'
        else: result += 'N,'
        # relationship status e.g. married. 
        if item.find('Relationship Status') >= 0: result += 'Y,'
        else: result += 'N,'
        # interested in e.g. women. 
        if item.find('Interested In') >= 0: result += 'Y,'
        else: result += 'N,'
        # biobliography bio. 
        if item.find('About ') >= 0: result += 'Y,'
        else: result += 'N,'
 
        # phylosophy. 
        item = items[9]
        if item.find('Favorite Quotations') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Religious Views') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Political Views') >= 0: result += 'Y,'
        else: result += 'N,'
 
        # friends. 
        item = items[6]
        if item.find('YES') >= 0: result += 'Y,'
        else: result += 'N,'
 
        # photos. 
        item = items[4]
        if item.find('YES') >= 0: result += 'Y,'
        else: result += 'N,'
        
        # wallpost. 
        item = items[2]
        if item.find('YES') >= 0: result += 'Y,'
        else: result += 'N,'
        
        # contact. 
        item = items[13]
        if item.find('Website') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Address') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Screen Name') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Email') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Facebook') >= 0: result += 'Y,'
        else: result += 'N,'
        if item.find('Phone') >= 0: result += 'Y,'
        else: result += 'N,'
        
        # profile. 
        item = items[7]
        # Married to
        if item.find('Married to') >= 0: result += 'Y,'
        else: result += 'N,'
        # DOB
        if item.find('Born on') >= 0: result += 'Y,'
        else: result += 'N,'
        # current location. 
        if item.find('Lives in') >= 0: result += 'Y,'
        else: result += 'N,'
        # home town
        if item.find('From ') >= 0: result += 'Y,'
        else: result += 'N,'
 
        # interests. 
        item = items[11] 
        if item.find('Activities') >= 0: result += 'Y,' 
        else: result += 'N,'
        if item.find('Interests') >= 0: result += 'Y,'
        else: result += 'N,' 
        
        # edu_work. 
        item = items[8] 
        if item.find('Employers') >= 0: result += 'Y,' 
        else: result += 'N,'
        if item.find('Grad School') >= 0: result += 'Y,'
        else: result += 'N,' 
        if item.find('College') >= 0: result += 'Y,' 
        else: result += 'N,'
        if item.find('High School') >= 0: result += 'Y'
        else: result += 'N' 
        
        # if len(result) >= 27 * 2: 
        print result

if __name__ == "__main__":
    if len(sys.argv) >= 3: 
        tobinary(sys.argv[1], sys.argv[2])
    else: 
        tobinary(sys.argv[1], 'test')
