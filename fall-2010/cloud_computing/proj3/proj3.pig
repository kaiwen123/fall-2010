--REGISTER ./tutorial.jar
book = LOAD 'data/book' USING PigStorage('\t') AS (isbn:chararray, name:chararray);
customer = LOAD 'data/customer' USING PigStorage('\t') AS (cid:chararray, name:chararray, age:int, city:chararray, sex:chararray);
purchase = LOAD 'data/purchase' USING PigStorage('\t') AS (year:int, cid:chararray, isbn:chararray, seller:chararray, price:int);

--1. In the past years, how much did each of the sellers earn?
grouppurchase = GROUP purchase BY seller;
sellerearn = FOREACH grouppurchase GENERATE group, SUM(purchase.price);
STORE sellerearn INTO 'data/sellerearn';
--dump sellerearn;
-- Results; 
--(Amazon,135L)
--(Borders,90L)
--(Barnes Nobel,30L)

--2. How much did people in each city buy books in the past years?
rawcitypurchase = JOIN purchase BY cid, customer BY cid;
groupcitypurchase = GROUP rawcitypurchase BY customer::city; 
citypurchase = FOREACH groupcitypurchase GENERATE group, SUM(rawcitypurchase.purchase::price);
STORE citypurchase INTO 'data/citypurchase';
--result;
--dump citypurchase;
--(Dayton,110L)
--(Beavercreek,145L)

--3. If categorized by gender, how much did each sex spend on buying books in the past years?
rawgenderpurchase = JOIN purchase BY cid, customer BY cid; 
groupgenderpurchase = GROUP rawgenderpurchase BY customer::sex;
genderpurchase = FOREACH groupgenderpurchase GENERATE group, SUM(rawgenderpurchase.purchase::price);
STORE genderpurchase INTO 'data/genderpurchase';
--result:
--dump genderpurchase; 
--(F,115L)
--(M,140L)

--4. Give the names of the books that Amazon gives the lowest price (for the books that have at least two sellers).
amazonbooks = FILTER purchase BY seller == 'Amazon';
samebooks = JOIN amazonbooks by isbn, purchase by isbn;
amazoncheaper = FILTER samebooks by (amazonbooks::price < purchase::price);
cheaperbooks = JOIN amazoncheaper BY amazonbooks::isbn, book BY isbn;
cheaperout = FOREACH cheaperbooks generate amazonbooks::isbn, book::name, amazonbooks::price;
STORE cheaperout INTO 'data/amazoncheap';
--dump cheaperout;
--result:
--(B2,Drama,25)

cat data/citypurchase
cat data/genderpurchase
cat data/sellerearn
cat data/amazoncheap
