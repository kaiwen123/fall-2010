-- This SQL file is used to create the database and table used for the
-- project. 
-- There are three tables: 
-- CUSTOMER table which represents the customer. 
-- FOOD table represents a single food description. 
-- C_ORDER table represents food order of a customer. 
-- 
-- Created by Simon (gsmsteve@gmail.com) on 10:45AM 03/03/2011. 
-- All Rights Reserved. 

CREATE DATABASE IF NOT EXISTS DB_PROJ; 

USE DB_PROJ;

CREATE TABLE IF NOT EXISTS CUSTOMER (
CID INT, 
CFname VARCHAR(100), 
CLname VARCHAR(100), 
CAddress VARCHAR(100), 
CAge INT, 
CPhone VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS FOOD (
FID INT, 
FName VARCHAR(100), 
FCategory VARCHAR(100), 
FUcalorie INT
); 

-- OID The order id.
-- OCID the customer ID; 
-- OFID the food for this order; 
-- OTime Time of this order; 
-- OQuant Quantity of this order;
CREATE TABLE IF NOT EXISTS C_ORDER (
OCID INT,
OFID INT,
OTime TIMESTAMP,
OQuant INT
);