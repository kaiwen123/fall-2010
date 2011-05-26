import sys
def tobinary(filename):
    ''' This program transforms the profile output to binary format. '''
    pfile = open(filename, 'r')
    DEBUG_FORMAT1 = False
    DEBUG_FORMAT = False
    for line in pfile: 
        items = line.split('\t')

        # test the format is corrent. 
        # result = items[0] + ' '
        result = ''

        for item in items:
            if item.strip() == '': continue
            # find profile setting items.
            if item.find('basic_info:') >= 0:
                # sex
                if item.find('Sex') >= 0: result += 'sex '
                # networks
                if item.find('Networks') >= 0: result += 'network '
                # relationship status e.g. married. 
                if item.find('Relationship Status') >= 0: result += 'relstatus '
                # interested in e.g. women. 
                if item.find('Interested In') >= 0: result += 'interest '
                # biobliography bio. 
                if item.find('About ') >= 0: result += 'about '

            if item.find('philosophy:') >= 0: 
                if item.find('Favorite Quotations') >= 0: result += 'favorite '
                if item.find('Religious Views') >= 0: result += 'religiousview '
                if item.find('Political Views') >= 0: result += 'politicalview '

            if item.find('friends:') >= 0: 
                if item.find('YES') >= 0: result += 'friends '

            if item.find('photos:') >= 0: 
                if item.find('YES') >= 0: result += 'photos '
                
            if item.find('wallpost:') >= 0: 
                if item.find('YES') >= 0: result += 'wallposts '

            if item.find('contact:') >= 0:
                if item.find('Website') >= 0: result += 'website '
                if item.find('Address') >= 0: result += 'address '
                if item.find('Screen Name') >= 0: result += 'screenname '
                if item.find('Email') >= 0: result += 'email '
                if item.find('Facebook') >= 0: result += 'facebook '
                if item.find('Phone') >= 0: result += 'phone '

            if item.find('PROFILE:') >= 0:
                # Married to
                if item.find('Married to') >= 0: result += 'marriedto '
                # DOB
                if item.find('Born on') >= 0: result += 'birthday '
                # current location. 
                if item.find('Lives in') >= 0: result += 'livesin '
                # home town
                if item.find('From ') >= 0: result += 'hometown '

            if item.find('act_interests:') >= 0:
                if item.find('Activities') >= 0: result += 'activities ' 
                if item.find('Interests') >= 0: result += 'interests '

            if item.find('edu_work') >= 0:
                if item.find('Employers') >= 0: result += 'employers ' 
                if item.find('Grad School') >= 0: result += 'gradschool '
                if item.find('College') >= 0: result += 'college ' 
                if item.find('High School') >= 0: result += 'highschool '

        print result

if __name__ == "__main__":
    tobinary(sys.argv[1])
