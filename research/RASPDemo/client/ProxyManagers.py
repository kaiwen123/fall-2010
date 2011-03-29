import MySQLdb

class DBConnector:
    ''' This class manages the connection to 
    the user and the data store database. '''
    def __init__(self):
        ''' Data base connection management object. '''
        self.DB_HOST = 'localhost'
        self.DB_user = 'ada'
        self.DB_user_pwd = '825123'
        self.DB_name = 'rasp_demo'

    def connectDB(self):
        ''' Connect to database. ''' 
        try:
            conn = MySQLdb.connect(host=self.DB_HOST, user=self.DB_user,
                                   passwd=self.DB_user_pwd, db=self.DB_name)
            # cursor = conn.cursor()
            return conn # cursor
        except MySQLdb.Error, e:
            print 'Proxy Server Database connection error.'
            return None

class ProxyUserManager:
    ''' Management of users. ''' 
    def __init__(self):
        ''' Do some preparation work such as testing the user
        database. ''' 
        self.manage_db = DBConnector()
        
    def manageUserDb(self):
        ''' Management of user database. ''' 
        # Create database and create database table if not yet exist.
        try:
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            # create the data user table. 
            cursor = conn.cursor()
            cursor.execute('''CREATE TABLE IF NOT EXISTS user(
                  user_id varchar(20) not null, 
                  user_pwd varchar(20) not null,
                  primary key(user_id));''') 
        except MySQLdb.Error, e:
            print "Error %d: %s" % (e.args[0], e.args[1])
        finally:
            conn.close()            

    def userRegistration(self, username, passwd):
        ''' Handle registration request from the data user. '''
        print 'LOG --- User registration request. - ProxyUserManager.'
        try:
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            cursor.execute(''' SELECT * FROM user WHERE user_id = %s''', (username))
            data = cursor.fetchall()
            if len(data) != 0: 
                return 'User already exists, please choose a different username.' 
            else:
                try:
                    cursor.execute(''' INSERT INTO user (user_id, user_pwd)
                                   VALUES(%s, %s)''', (username, passwd))
                    conn.commit()
                    return 'Welcome,' + username + '!'
                except:
                    conn.rollback()
                    raise MySQLdb.Error('Database error while creating user: '+username)
                
        except MySQLdb.Error, e:
            print "Error %d: %s" % (e.args[0], e.args[1])   
            return 'Error happened during registration process...'
        finally:
            conn.close()

    def userLogin(self, username, passwd):
        ''' Handle user login request. ''' 
        print 'LOG --- User login - ProxyUserManager. '
        try:
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            cursor.execute(''' SELECT * FROM user WHERE user_id = %s
                           AND user_pwd = %s''', (username, passwd))
            data = cursor.fetchall()
            if len(data) != 1:
                return 'No such user, please register before login.'
            else:
                return 'Welcome back ' + username
        except MySQLdb.Error, e:
            print "Error %d: %s" % (e.args[0], e.args[1])
            return 'Error happened during login process...'
        finally:
            conn.close()

class ProxyDataManager:
    ''' Management of user data, such as data stores a user have, and 
    the creation, updating and deletion of these data stores. 

    Data stores are managed according to their dimension, so, for a
    specific user, there are data stores with different dimension. If
    a user creates data with dimension DIM, the proxy server will
    first poke with the data manager to see if data with this
    dimension already exists, if yes, just upload data and append data
    to the existing data store, if not, new data store needs to be
    created and synchronized with the data server on the cloud. 

    Similarly, for data query requests from data owner, if there is no
    data store with this dimension exists, the query will be cancelled
    with a messge to the data owner showing that no such data store
    exists. ''' 
    def __init__(self):
        ''' initialize the connection to the database. '''
        self.manage_db = DBConnector()
        
    def manageUserDataDb(self):
        ''' Management of user data's database. ''' 
        try:
            conn = self.manage_db.connectDB() # connection to the database. 
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            cursor.execute('''CREATE TABLE IF NOT EXISTS data (
                  user_id varchar(20) not null, 
                  data_dim int default 2, 
                  data_file_name varchar(20) not null,
                  key_string varchar(10000) not null,
                  data_record_cnt int,
                  primary key(user_id, data_dim));''') 
        except MySQLdb.Error, e:
            print "Error %d: %s" % (e.args[0], e.args[1])
        finally:
            conn.close()            

    def createDataStore(self, username, dim):
        ''' create data store and push the store to the data
        server. ''' 
        self.manageUserDataDb()
        print 'LOG --- User Data Store Creation request. - ProxyDataManager.'
        try:
            #self.manageUserDataDb()
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            sql = "SELECT * FROM data WHERE user_id = '%s' \
                   AND data_dim = '%d'" % (username, dim)
            cursor.execute(sql)
            data = cursor.fetchall()
            if len(data) != 0: 
                return 'Data store already exists, you can upload data to it now.' 
            else:
                try:
                    db_name = username + '_' + str(dim)
                    sql = "INSERT INTO data (user_id, data_dim, data_file_name, key_string, \
                        data_record_cnt) VALUES('%s', '%d', '%s', '%s', '%d')" % \
                        (username, dim, db_name, '(A,v)', 10)
                    cursor.execute(sql)
                    conn.commit()        
                    return 'Successfully create,' + db_name + '!'
                except:
                    conn.commit()
                    raise MySQLdb.Error('Database error while creating data store: ', db_name)
        except MySQLdb.Error, e:
            print "Error %s: %s" % (e.args[0], e.args[1])   
            return 'Error happened during registration process...'        
        finally:
            conn.close()

    def deleteDataStore(self, username, dim):
        ''' create data store and push the store to the data
        server. ''' 
        self.manageUserDataDb()
        print 'LOG --- User Data Store Delete request. - ProxyDataManager.'
        try:
            #self.manageUserDataDb()
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            sql = "DELETE FROM data WHERE user_id = '%s' \
                   AND data_dim = '%d'" % (username, dim)
            cursor.execute(sql)
            conn.commit()
            return 'Successfully deleted data store record' + username + '_' + str(dim)
        except MySQLdb.Error, e:
            conn.rollback()
            print "Error %s: %s" % (e.args[0], e.args[1])   
            return 'Error happened during deletion process...'
        finally:
            conn.close()

    def listDataStores(self, username):
        ''' List all the available data stores for a specific user. '''
        print 'LOG --- User Data Store list. - ProxyDataManager.'
        try:
            conn = self.manage_db.connectDB()
            if conn == None: raise MySQLdb.Error('DB connection error! ') 
            cursor = conn.cursor()
            cursor.execute(''' SELECT * FROM data WHERE user_id = %s''', (username))
            stores = cursor.fetchall()
            store_str = '' 
            for store in stores:
                store_str += store[2] + ' ' + str(store[4]) + '\n'
            return store_str
        except MySQLdb.Error, e:
            print "Error %d: %s" % (e.args[0], e.args[1])   
            return 'Error happened during data store query process...'
        finally:
            conn.close()
        
if __name__ == '__main__':
    user_manager = ProxyUserManager()
    data_manager = ProxyDataManager()
    print user_manager.userRegistration('hello', '123')
    print user_manager.userLogin('hello', '1235')
    for i in range(1, 10):
        print data_manager.createDataStore('simon', i)
        print data_manager.createDataStore('ada', i)

    for i in range(6, 10):
        print data_manager.deleteDataStore('ada', i)
    print data_manager.listDataStores('simon')
    print data_manager.listDataStores('ada')
