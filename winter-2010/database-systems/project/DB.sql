-- This SQL file is used to create the database and table used for the
-- project. 
-- There are three tables: 
-- Customer table which represents the customer. 
-- Food table represents a single food description. 
-- FOrder table represents food order of a customer. 
-- 
-- Created by Simon (gsmsteve@gmail.com) on 10:45AM 03/03/2011. 
-- All Rights Reserved. 

CREATE DATABASE IF NOT EXISTS DB_PROJ; 

USE DB_PROJ;

-- CID the customer id, - primary key. 
-- CFname the first name of the customer; 
-- CLname the last name of the customer; 
-- CAddress the address of the customer; 
-- CAge the age of the customer; 
-- CPhone the phone of the customer. 
CREATE TABLE IF NOT EXISTS Customer (
CID VARCHAR(10) NOT NULL, 
CFname VARCHAR(100) NOT NULL, 
CLname VARCHAR(100) DEFAULT NULL, 
CAddress VARCHAR(100) DEFAULT NULL, 
CAge INT DEFAULT 0, 
CPhone VARCHAR(100) DEFAULT NULL,
PRIMARY KEY (CID)
)ENGINE=INNODB;

-- FID the food id; 
-- FName the descriptive name of the food; 
-- FCategory which category does this food belong to?
-- FUcalorie The unit calorie of the food. 
CREATE TABLE IF NOT EXISTS Food (
-- FID INT NOT NULL AUTO_INCREMENT, 
FName VARCHAR(20) NOT NULL, 
FCategory VARCHAR(100) DEFAULT "American", 
FUcalorie INT DEFAULT 0,
PRIMARY KEY (FName)
)ENGINE=INNODB; 

-- OID The order id.
-- OCID the customer ID; 
-- OFID the food for this order; 
-- OTime Time of this order; 
-- OQuant Quantity of this order;
CREATE TABLE IF NOT EXISTS FOrder (
OID INT NOT NULL AUTO_INCREMENT,
OCID VARCHAR(10),
OFName VARCHAR(20),
OTime TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
OQuant INT DEFAULT 1,
PRIMARY KEY (OID),
FOREIGN KEY (OCID) REFERENCES Customer(CID)
	ON DELETE CASCADE ON UPDATE CASCADE,
FOREIGN KEY (OFName) REFERENCES Food(FName)
	ON DELETE CASCADE ON UPDATE CASCADE
)ENGINE=INNODB;