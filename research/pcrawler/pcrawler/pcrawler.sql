-- This is the database file for java social network crawler. 
CREATE DATABASE IF NOT EXISTS pcrawler; 

USE pcrawler;

-- users_tocrawl.
CREATE TABLE IF NOT EXISTS users_tocrawl (
   userID VARCHAR(255) NOT NULL, 
   fromID VARCHAR (255) NOT NULL, 
   description TEXT 
)ENGINE=INNODB; 

-- users_crawled.
CREATE TABLE IF NOT EXISTS users_crawled (
   userID VARCHAR(255) NOT NULL, 
   fromID VARCHAR (255) NOT NULL, 
   description TEXT 
)ENGINE=INNODB; 