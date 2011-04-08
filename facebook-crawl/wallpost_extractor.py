import mechanize, re
from BeautifulSoup import BeautifulSoup, NavigableString, UnicodeDammit

def extractWallPosts(filename):
    ''' Get wall posts for the friends starting from
    friend_id. '''
    wallposts = ''
    wallsoup = BeautifulSoup(''.join(open(filename).read()))
    wallposts_list = wallsoup.find(name='ul',attrs={'id':'profile_minifeed'})
    posts = wallposts_list.findAll('li',{'class':re.compile('pvm*')})
    events = ''         # post events.
    for post in posts:
        if post.has_key('class'):
            print post['class']
        continue
        datetime = post.find('abbr')['data-date'] # date and time.
        # Author of the post, might be like events. 
        ''' There are several types of events for the wall post. 
        First, [author] do something; 
        Second, [some-friend] likes something; 
        Third, [some-friend] was tagged by [somebody] at [somewhere]; 
        Fourth, [some-body] commented on [some-body]'s content;
        Fifth, TO be added. '''
        author = post.find('div',{'class':re.compile('actorName*')})
        wallposts += ' | ' + events
        events = ''
        if author == None:           
            passive = post.find('h6').a
            p = passive.string
            try:            # likes and dislikes and comments. 
                a = passive.nextSibling.nextSibling.string
                events = p+' likes/commented on '+ a +'\'s post/photo!'
            except:         # tagging event. 
                a = passive.nextSibling.nextSibling.nextSibling.nextSibling.string
                events = p+' was tagged on '+a+'\'s photo!'
        else:               # [author] do something.
            if author.a.contents != None:
                events = author.a.string
        # Message contents. 
        message = post.find('span')
        if message.string != None:
            events += " MESSAGE: " + message.string
        elif message.contents != []:
            if isinstance(message.contents[0], str): 
                events += " MESSAGE: " + message.contents[0]

    print wallposts 

if __name__ == '__main__':
    getWallPosts('wallpost1.html')
