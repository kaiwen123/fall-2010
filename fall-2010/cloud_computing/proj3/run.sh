# Delete result files first to avoid errors. 
rm -rf data/citypurchase
rm -rf data/genderpurchase
rm -rf data/sellerearn
rm -rf data/amazoncheap

# Run pig script. 
pig proj3.pig

# Check result WITHIN Pig. 
#cat data/citypurchase
#cat data/genderpurchase
#cat data/sellerearn
#cat data/amazoncheap
