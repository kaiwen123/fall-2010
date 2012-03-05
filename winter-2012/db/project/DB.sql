-- Database tables for CEG 701 Database principles project. 
-- Author: Shumin Guo. 

CREATE DATABASE IF NOT EXISTS DB_PROJ; 

USE DB_PROJ;

CREATE TABLE IF NOT EXISTS BankAccount (
   AccntId VARCHAR(20) NOT NULL, 
   AccntName VARCHAR(20) NOT NULL, 
   memo VARCHAR(255), 
   balance DOUBLE,--updated by operation. 
   opendate DATE NOT NULL, 
   PRIMARY KEY (AccntId)
)ENGINE=INNODB; 

CREATE TABLE IF NOT EXISTS Income (
   IncomeId VARCHAR(20) NOT NULL, 
   IncomeDate DATE NOT NULL,
   amount DOUBLE NOT NULL,
   description VARCHAR(255), 
   saveToAccnt VARCHAR(20) NOT NULL, 
   PRIMARY KEY (IncomeID),
   FOREIGN KEY (saveToAccnt) REFERENCES BankAccount(AccntId)
	ON DELETE CASCADE ON UPDATE CASCADE,
)ENGINE=INNODB;

CREATE TABLE IF NOT EXISTS Spend (
   SpendDate DATE NOT NULL, 
   location VARCHAR(20) NOT NULL, 
   forwhat VARCHAR(100), 
   chargefromAccnt VARCHAR(20) NOT NULL, 
   memo VARCHAR(255),
   FOREIGN KEY (chargefromAccnt) REFERENCES BankAccount(AccntId)
	ON DELETE CASCADE ON UPDATE CASCADE,
)ENGINE=INNODB; 

CREATE TABLE IF NOT EXISTS Budget (
   BudgetId VARCHAR(20) NOT NULL, 
   BudgetFor VARCHAR(20) NOT NULL, 
   CreateDate DATE, 
   BudgetFromAccnt VARCHAR(20), 
   Amount DOUBLE NOT NULL DEFAULT 0, 
   Memo VARCHAR(255),
   PRIMARY KEY (BudgetId),
   FOREIGN KEY (BudgetFromAccnt) REFERENCES BankAccount(AccntId)
	ON DELETE CASCADE ON UPDATE CASCADE,
)ENGINE=INNODB; 

CREATE TABLE IF NOT EXISTS Debt (
   DebtId VARCHAR(20) NOT NULL, 
   DebtType VARCHAR(10) NOT NULL, -- borrow/lend. 
   DebtStartDate DATE NOT NULL, 
   DebtEndDate DATE NULL NULL, 
   DebtAmount DOUBLE NOT NULL, 
   Memo VARCHAR(255), 
   PRIMARY KEY (DebtId)
)ENGINE=INNODB; 
