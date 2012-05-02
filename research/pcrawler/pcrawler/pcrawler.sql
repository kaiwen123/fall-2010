-- This is the database file for java social network crawler. 
CREATE DATABASE IF NOT EXISTS qcrawler; 

USE qcrawler;

-- users_tocrawl.
CREATE TABLE IF NOT EXISTS users_tocrawl (
   userID VARCHAR(20) NOT NULL, 
   fromID VARCHAR (20) NOT NULL, 
   description TEXT 
)ENGINE=INNODB; 

-- users_crawled.
CREATE TABLE IF NOT EXISTS users_crawled (
   userID VARCHAR(20) NOT NULL, 
   fromID VARCHAR (20) NOT NULL, 
   description TEXT 
)ENGINE=INNODB; 