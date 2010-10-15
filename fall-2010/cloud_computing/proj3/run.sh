# Delete result files first to avoid errors. 
hadoop fs -rmr /user/w234sxg/prj3/citypurchase
hadoop fs -rmr /user/w234sxg/prj3/genderpurchase
hadoop fs -rmr /user/w234sxg/prj3/sellerearn
hadoop fs -rmr /user/w234sxg/prj3/amazonlowest

# Run pig script. 
pig proj3.pig
