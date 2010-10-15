book = LOAD 'prj3/book' USING PigStorage('\t') AS (isbn:chararray, name:chararray);
customer = LOAD 'prj3/customer' USING PigStorage('\t') AS (cid:chararray, name:chararray, age:int, city:chararray, sex:chararray);
purchase = LOAD 'prj3/purchase' USING PigStorage('\t') AS (year:int, cid:chararray, isbn:chararray, seller:chararray, price:int);

--1. In the past years, how much did each of the sellers earn?
grouppurchase = GROUP purchase BY seller;
sellerearn = FOREACH grouppurchase GENERATE group, SUM(purchase.price);
STORE sellerearn INTO 'prj3/sellerearn';
--dump sellerearn;
-- Results; 
--(Amazon,135L)
--(Borders,90L)
--(Barnes Nobel,30L)

--2. How much did people in each city buy books in the past years?
rawcitypurchase = JOIN purchase BY cid, customer BY cid;
groupcitypurchase = GROUP rawcitypurchase BY customer::city; 
citypurchase = FOREACH groupcitypurchase GENERATE group, SUM(rawcitypurchase.purchase::price);
STORE citypurchase INTO 'prj3/citypurchase';
--result;
--dump citypurchase;
--(Dayton,110L)
--(Beavercreek,145L)

--3. If categorized by gender, how much did each sex spend on buying books in the past years?
rawgenderpurchase = JOIN purchase BY cid, customer BY cid; 
groupgenderpurchase = GROUP rawgenderpurchase BY customer::sex;
genderpurchase = FOREACH groupgenderpurchase GENERATE group, SUM(rawgenderpurchase.purchase::price);
STORE genderpurchase INTO 'prj3/genderpurchase';
--result:
--dump genderpurchase; 
--(F,115L)
--(M,140L)

--4. Give the names of the books that Amazon gives the lowest price (for the books that have at least two sellers).
groupedpurchase = GROUP purchase BY isbn;
filteredpurchase = FILTER groupedpurchase BY COUNT(purchase) > 1;
minprice = FOREACH filteredpurchase GENERATE purchase, MIN(purchase.price) as minp;
flattened = FOREACH minprice GENERATE flatten(purchase), minp;
filteredhighprice = FILTER flattened BY price <= minp;
uniqsellerforbook1 = FOREACH filteredhighprice GENERATE isbn, seller, price, minp;
uniqsellerforbook2 = DISTINCT uniqsellerforbook1;
countlowestpricesellers = GROUP uniqsellerforbook2 BY isbn;
lowestsellers = FILTER countlowestpricesellers BY COUNT(uniqsellerforbook2) == 1;
flattenseller = FOREACH lowestsellers GENERATE flatten($1);
amazonislowest = FILTER flattenseller BY seller == 'Amazon';
amazonlowestwithbooknames = JOIN amazonislowest BY isbn, book BY isbn;
cleanedresult = FOREACH amazonlowestwithbooknames GENERATE name, seller, isbn, price;
STORE cleanedresult INTO 'prj3/amazonlowest';

--Display results for all questions.
cat prj3/citypurchase
cat prj3/genderpurchase
cat prj3/sellerearn
cat prj3/amazonlowest
