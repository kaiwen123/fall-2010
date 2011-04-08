import re, mechanize, time, Queue, codecs, sys, logging
from BeautifulSoup import BeautifulSoup, NavigableString, UnicodeDammit
# import related modules. 
from frendlist_extractor import buildSocialGraph
from profile_extractor import extractProfile
from wallpost_extractor import extractWallPosts
          
class crawler:
    ''' This the facebook crawler class. It will download facebook pages 
    and parse the source html file and output the desired information. ''' 

    def __init__(self):
        ''' Initilize the crawler. ''' 
        self.CRAWLE_COUNT = 10000 # how many users to crawl? 
        self.social_graph = codecs.open('social_graph.txt', 'a', encoding='utf-8')
        self.__PRINT_FRIEND_GRAPH__ = 0 # for debuggin purpose. 
        self.browser = mechanize.Browser()
        self.browser.addheaders = [('User-Agent', 'Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.16) Gecko/20110322 Fedora/3.6.16-1.fc14 Firefox/3.6.16')]
        self.browser.set_handle_equiv(True)
        self.browser.set_handle_redirect(True)
        self.browser.set_handle_refresh(False)
        self.browser.set_handle_gzip(False)
        self.browser.set_debug_redirects(True)
        self.browser.set_handle_robots(False)
        self.browser._factory.is_html = True
        self.linkqueue = Queue.Queue() 
        initialFeedId = 'shumin.guo' # '100000458708905' if you don't have name, use id instead. 
        self.linkqueue.put(initialFeedId)
        self.friendlinks = {initialFeedId:'Initial'}
        self.logger = logging.getLogger("mechanize_redirects")
        self.logger.addHandler(logging.StreamHandler(sys.stdout))
        self.logger.setLevel(logging.INFO)
        print "Crawler initilized ...... " 

    def login(self):
        ''' Log onto facebook with a facebook account. '''
        self.browser.open('https://login.facebook.com/login.php')
        self.browser.select_form(nr=0)
        self.browser['email'] = 'gsmsteve@gmail.com' # 'gsm1011@163.com' #'gsmsteve@gmail.com'
        self.browser['pass'] = 'Gsm1011!' # 'changeme123' # 'Gsm1011!'
        response = self.browser.submit() 
        if response != None:
            print "Logged into facebook ...... "
        else:
            print 'Facebook login failed ...... '
        return response 

    def getProfileLink(self, friend_id):
        ''' Return the profile link of a friend according to the friend id. ''' 

        if self.friendlinks.has_key(friend_id): 
            if re.compile('\D').findall(friend_id) == []: # feed id is numerical.
                profileurl = 'http://www.facebook.com/profile.php?id=' + friend_id
            else:
                profileurl = 'http://www.facebook.com/' + friend_id # feed id is alphabetical.
            return profileurl
        else:
            return ''


    def loadSocialGraph(self, file_name):
        ''' Load the social graph from a given file. '''
        graph = codecs.open(file_name, 'r', encoding='utf-8')
        for line in graph:
            # current user and source user. 
            cur_user = line.split(" ")[0]
            src_user = line.split(" ")[1]
            print "Loaded friend", cur_user, "from friend", src_user
            if not self.friendlinks.has_key(cur_user):
                self.linkqueue.put(cur_user)
                self.friendlinks[cur_user] = src_user 
        
    def doCrawl(self):
        # The following block is commented because I just want to read in the social graph 
        # and then do the crawling, if you want to build a new social graph, please uncomment this block. 
        ''' This is the scheduler of the crawler. 
        It first builds a friend list to be crawled, and then crawle the 
        profiles, wallposts etc. The crawling can be multi-threaded. ''' 
        self.loadSocialGraph('social_graph.txt') 
        self.login()
        # First step: build the social graph. 
        '''while self.linkqueue.qsize() > 0:
            if len(self.friendlinks) >= self.CRAWLE_COUNT:
                break
            friend = self.linkqueue.get()
            print "Crawling user :", friend, ", TODO: ", self.linkqueue.qsize(), " TOTAL: ", len(self.friendlinks) 

            # try two times. 
            if not self.buildSocialGraph(friend): 
                time.sleep(5)
                self.buildSocialGraph(friend)
            # Extract the profiles of user. 
            profiles = self.getFriendProfiles(friend)
            resultstr = "{" + friend + "}{" + profiles + "}"
            print resultstr
        self.social_graph.close() '''

        # Second step: Crawl the facebook user profiles. 
        user_profiles = codecs.open('user_profiles.txt','a', encoding='utf-8')
        for friend in self.friendlinks.keys():
            profiles = self.getFriendProfiles(friend)
            resultstr = "{" + friend + "}{" + profiles + "}"
            user_profiles.write(unicode(resultstr) + '\n')
            user_profiles.flush()
        user_profiles.close() 

        # Third step: Crawl the facebook user wallposts.  
        user_wallposts = codecs.open('user_wallposts.txt','a', encoding='utf-8')
        for friend in self.friendlinks.keys():
            wallposts = self.getWallPosts(friend)
            wallposts.write(unicode(wallposts))
            wallposts.flush()
        user_wallposts.close()
            
if __name__ == "__main__":
    fbcrawler = crawler() 
    fbcrawler.doCrawl()

    # TODO. Debug to crawl the content of the profile page. 
