import mechanize,re 
from BeautifulSoup import BeautifulSoup, NavigableString, UnicodeDammit

def buildSocialGraph(filename):
    friendsoup = BeautifulSoup(''.join(open(filename).read()))

    # print re.compile('Friends \(\d+\)').search(''.join(open(filename).read())).string
    # print re.sub(pattern='Friends \(\d+\)', repl = '', string = str(friendsoup))

    friends = friendsoup.html.body.findAll(name='div')
    friends1 = BeautifulSoup(''.join(str(friends)))
    friend_div = friends1.find(name='div',attrs={'id':'pagelet_main_column_personal'})
    # print friend_div
    if friend_div == None: return False # friend list not public. 
    friends = friend_div.findAll('div',{'class':'fsl fwb fcb'})
    if friends != []:     # friends in a list. 
        for friend in friends:
            friend_name = friend.a.string 
            friend_link = friend.a['href']
            id_pos = friend_link.rfind('=')
            if id_pos == -1:    # link format: http://www.facebook.com/farrahwu
                slash_pos = friend_link.rfind('/')
                friend_id = friend_link[slash_pos+1:len(friend_link)]
            else:               # link format: http://www.facebook.com/link.php?id=65266
                friend_id = friend_link[id_pos+1:len(friend_link)]
            if friend.nextSibling != None: 
                friend_name += '(' + friend.nextSibling.string + ')'
            print friend_id, friend_name

            ''' Save result to social graph. 
            if not self.friendlinks.has_key(friend_id): 
                self.friendlinks[friend_id] = fromfriend
                self.linkqueue.put(friend_id)
                print unicode(friend_name)
                # self.social_graph.write(friend_id+" "+fromfriend +" "+unicode(friend_name)+"\n")
                # self.social_graph.flush()
                if self.__PRINT_FRIEND_GRAPH__ == 1: 
                    print friend_id, self.friendlinks[friend_id]
           '''
    else:                   # friends in a table. 
        friends = friend_div.findAll('div',{'class':'photoWrapper'})
        for friend in friends:
            friend_name = friend.nextSibling.string
            if friend_name.find('Suggest Friends') != -1: continue
            friend_link = friend.parent['href']
            id_pos = friend_link.rfind('=')
            if id_pos == -1:    # link format: http://www.facebook.com/farrahwu
                slash_pos = friend_link.rfind('/')
                friend_id = friend_link[slash_pos+1:len(friend_link)]
            else:               # link format: http://www.facebook.com/link.php?id=65266
                friend_id = friend_link[id_pos+1:len(friend_link)]

            ''' Save result to social graph.
            if not self.friendlinks.has_key(friend_id): 
                friendlinks[friend_id] = fromfriend
                linkqueue.put(friend_id)
                print unicode(friend_name)
                # self.social_graph.write(friend_id+" "+fromfriend +" "+unicode(friend_name)+"\n")
                # self.social_graph.flush()
                print friend_id, fromfriend
            '''
            print friend_id, friend_name
    return True

def testVisibleItems(filename):
    ''' test which items are visible? (wall, info, photo, notes and
    friends)'''  
    friendsoup = BeautifulSoup(''.join(open(filename).read()))
    friends = friendsoup.html.body.findAll(name='div')
    friends1 = BeautifulSoup(''.join(str(friends)))
    friend_div = friends1.find(name='div',attrs={'id':'pagelet_left_column'})

    # Test if items are visible?
    wall = friend_div.find('li',{'id':'navItem_wall'})
    info = friend_div.find('li',{'id':'navItem_info'})
    photos = friend_div.find('li',{'id':'navItem_photos'})
    notes = friend_div.find('li',{'id':'navItem_notes'})
    friends = friend_div.find('li',{'id':'navItem_friends'})
    
    # How many friends does this user have ?
    friend_count = friend_div.find('div',{'id':'pagelet_relationships'})
    fcount = ''
    if friend_count == None: 
        fcount = 'friendcount:N/A\t'
    else:
        friends_link = friend_count.find('span',{'class':'fcg'})
        counttext = friends_link.a.string 
        idx1 = counttext.find('(')
        idx2 = counttext.find(')')
        count = counttext[idx1+1:idx2]
        fcount = 'friendcount:'+count + '\t'
    
    # YES means visible and NO means not. 
    visibility = fcount 
    if wall == None: visibility += 'wallpost:NO\t'
    else: visibility += 'wallpost:YES\t'
    if info == None: visibility += 'info:NO\t'
    else: visibility += 'info:YES\t'
    if photos == None: visibility += 'photos:NO\t'
    else: visibility += 'photos:YES\t'
    if notes == None: visibility += 'notes:NO\t'
    else: visibility += 'notes:YES\t'
    if friends == None: visibility += 'friends:NO'
    else: visibility += 'friends:YES'
    print visibility

if __name__ == '__main__':
    # print "Extract friends"
    #buildSocialGraph('friend_list.html')
    #buildSocialGraph('friend_table.html')
    testVisibleItems('friend_list.html')
    testVisibleItems('friend_list2.html')
    testVisibleItems('friend_list3.html')
