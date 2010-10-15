# Delete result files first to avoid errors. 
rm -rf /home/w234sxg/project3/prj3/citypurchase
rm -rf /home/w234sxg/project3/prj3/genderpurchase
rm -rf /home/w234sxg/project3/prj3/sellerearn
rm -rf /home/w234sxg/project3/prj3/amazonlowest
# Run pig script. 
pig -x local proj3.pig
