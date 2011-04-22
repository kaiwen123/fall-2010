import re, mechanize, time, Queue, codecs, sys, logging, random
from BeautifulSoup import BeautifulSoup, NavigableString, UnicodeDammit
# import related modules. 
from friendlist_extractor import buildSocialGraph, testVisibleItems
from profile_extractor import extractProfile
from wallpost_extractor import extractWallPosts
          
class crawler:
    ''' This the facebook crawler class. It will download facebook pages 
    and parse the source html file and output the desired information. ''' 

    def __init__(self):
        ''' Initilize the crawler. ''' 
        self.CRAWLE_COUNT = 10000 # how many users to crawl? 
        self.social_graph = codecs.open('social_graph.txt', 'a', encoding='utf-8')
        self.__PRINT_FRIEND_GRAPH__ = 0 # for debug in purpose. 
        self.browser = mechanize.Browser()
        self.browser.addheaders = [('User-Agent', 'Googlebot')]#Googlebot
        self.browser.set_handle_equiv(False)
        self.browser.set_handle_redirect(True)
        self.browser.set_handle_robots(False)
        self.browser._factory.is_html = True
        self.linkqueue = Queue.Queue() 
        self.initialFeedId = 'shumin.guo' # username or id. 
        self.friendlinks = {self.initialFeedId:'Initial'}
        self.logger = logging.getLogger("mechanize_redirects")
        self.logger.addHandler(logging.StreamHandler(sys.stdout))
        self.logger.setLevel(logging.INFO)
        print "Crawler initilized ...... " 

    def login(self):
        ''' Log onto facebook with a facebook account. '''
        self.browser.set_handle_equiv(False)
        self.browser.set_handle_robots(False)
        self.browser.open('https://login.facebook.com/login.php')
        self.browser._factory.is_html = True
        self.browser.select_form(nr=0)
        self.browser['email'] = 'gsmsteve@gmail.com' # change to your facebook email. 
        self.browser['pass'] = 'Gsm1011!' # change to your facebook password. 
        response = self.browser.submit() 
        if response != None:
            print "Logged into facebook ...... "
        else:
            print 'Facebook login failed ...... '
        return response 

    def getLink(self, friend_id, id_type):
        ''' Return the profile link of a friend according to the friend id. ''' 

        if self.friendlinks.has_key(friend_id): 
            if re.compile('\D').findall(friend_id) == []: # feed id is numerical.
                profileurl = 'http://www.facebook.com/profile.php?id=' + friend_id + '&sk=' + id_type
            else:               # feed id is alphabetical.
                profileurl = 'http://www.facebook.com/' + friend_id + '?sk=' + id_type
            return profileurl
        else:
            return ''

    def getFriendCount(self):
        ''' How many friends does a user have? '''
        friend_count = self.browser.find_link(text_regex=re.compile('Friends ([0-9]+)'), nr=2)
        fcount = ''
        if friend_count == None: 
            fcount = 'friendcount:N/A\t'
        else:
            friends_link = friend_count.find('span',{'class':'fcg'})
            count = 'N/A'
            if friends_link != None:
                counttext = friends_link.a.string 
                # there might be other people other than friends. 
                if counttext.find('Friends (') >= 0:
                    idx1 = counttext.find('(')
                    idx2 = counttext.find(')')
                    if idx1 >= 0 and idx2 >= 2: 
                        count = counttext[idx1+1:idx2]
                        if count == '':
                            count = 'N/A'
            fcount = 'friendcount:'+count + '\t'
        
        return fcount 

    def loadSocialGraph(self, file_name):
        ''' Load the social graph from a given file. '''
        graph = codecs.open(file_name, 'r', encoding='utf-8')
        for line in graph:
            # current user and source user. 
            cur_user = line.split(" ")[0]
            src_user = line.split(" ")[1]
            print "Loaded", cur_user, "from friend", src_user
            if not self.friendlinks.has_key(cur_user):
                self.linkqueue.put(cur_user)
                self.friendlinks[cur_user] = src_user 

    def doCrawl(self):
        # The following block is commented because I just want to read in the social graph 
        # and then do the crawling, if you want to build a new social graph, please uncomment this block. 
        ''' This is the scheduler of the crawler. 
        It first builds a friend list to be crawled, and then crawle the 
        profiles, wallposts etc. The crawling can be multi-threaded. ''' 
        doBuildGraph = False
        doVisibilityTest = True
        doGetProfile = True
        doGetWallposts = True
        self.loadSocialGraph('social_graph.txt') 
        if self.linkqueue.qsize() == 0: 
            self.linkqueue.put(self.initialFeedId)
        self.login()
        user_profiles = codecs.open('user_profiles.txt','a', encoding='utf-8')
        user_wallposts = codecs.open('user_wallposts.txt','a', encoding='utf-8')
        # First step: build the social graph. 
        while self.linkqueue.qsize() > 0:
            if len(self.friendlinks) - self.linkqueue.qsize() >= self.CRAWLE_COUNT:
                break
            friend = self.linkqueue.get()
            print "Crawling user :", friend, ", TODO: ", self.linkqueue.qsize(), " TOTAL: ", len(self.friendlinks) 
            
            # First, get current user profile. 
            if doGetProfile == True:
                url = self.getLink(friend, 'info')
                profilepage = ''.join(self.browser.open(url).read())
                profiles = extractProfile(profilepage)
                if profiles == None or profiles == 'failed': 
                    profilepage = ''.join(self.browser.open(url).read())
                    profiles = extractProfile(profilepage)
                    
            # Second, Test left column item visibility. 
            if doVisibilityTest == True: 
                url = self.getLink(friend, 'friends')
                listpage = ''.join(self.browser.open(url).read())
                visibility = testVisibleItems(listpage)
                resultstr = friend + '\t' + visibility + '\t' + profiles
                user_profiles.write(unicode(resultstr) + '\n')
                user_profiles.flush()

            # Build social graph to crawl more users. 
            if doBuildGraph == True:
                if not buildSocialGraph(friend, listpage, self.linkqueue, \
                                            self.friendlinks, self.social_graph): 
                    time.sleep(5)
                    buildSocialGraph(friend, listpage, self.linkqueue, \
                                         self.friendlinks, self.social_graph)

            # Third, Crawl the wall post of current user. 
            if doGetWallposts == True:
                url = self.getLink(friend, 'wall')
                response = self.browser.open(url)
                # TODO: how to get wallposts as many as we can??? 
                wallpage = ''.join(response.read())
                wallposts = extractWallPosts(wallpage)
                user_wallposts.write(unicode(wallposts))
                user_wallposts.flush()

        user_profiles.close() 
        user_wallposts.close()
        self.social_graph.close() 
        return 
            
if __name__ == "__main__":
    fbcrawler = crawler() 
    fbcrawler.doCrawl()

    # TODO. Debug to crawl the content of the profile page. 
