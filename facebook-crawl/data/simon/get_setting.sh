# remove existing files. 
rm -rf *.csv

if [ $# -lt 1 ] 
then 
    echo 'Error, please enter your account name!! '; 
    exit; 
fi
acnt=$1
nbdy=nobody

# generate visibility setting from crawled raw data. 
python ../../python/tobinary.py $acnt.$acnt.f.tab.txt > $acnt.$acnt.f.vis.csv
python ../../python/tobinary.py $acnt.$acnt.fof.tab.txt > $acnt.$acnt.fof.vis.csv
python ../../python/tobinary.py $acnt.nobody.fof.tab.txt > $acnt.nobody.fof.vis.csv
python ../../python/tobinary.py $acnt.nobody.f.tab.txt > $acnt.nobody.f.vis.csv

# visibility with ids. 
python ../../python/tobinary.py $acnt.$acnt.f.tab.txt id > $acnt.$acnt.f.vis-id.csv
python ../../python/tobinary.py $acnt.$acnt.fof.tab.txt id > $acnt.$acnt.fof.vis-id.csv
python ../../python/tobinary.py $acnt.nobody.fof.tab.txt id > $acnt.nobody.fof.vis-id.csv
python ../../python/tobinary.py $acnt.nobody.f.tab.txt id > $acnt.nobody.f.vis-id.csv

files=`ls *csv`

# combine setting according to rule and generate. 
cp ../column.names.txt $acnt.combine.f.priv-mul-setting.csv
cp ../column.names.txt $acnt.combine.fof.priv-mul-setting.csv

cp ../column.names.txt $acnt.combine.f.priv-bin-setting.csv
cp ../column.names.txt $acnt.combine.fof.priv-bin-setting.csv

python ../../python/tomulti.py $acnt.$acnt.f.vis-id.csv $acnt.nobody.f.vis-id.csv >> $acnt.combine.f.priv-mul-setting.csv
python ../../python/tomulti.py $acnt.$acnt.fof.vis-id.csv $acnt.nobody.fof.vis-id.csv >> $acnt.combine.fof.priv-mul-setting.csv

python ../../python/tomulti-bin1.py $acnt.$acnt.f.vis-id.csv $acnt.nobody.f.vis-id.csv >> $acnt.combine.f.priv-bin-setting.csv
python ../../python/tomulti-bin1.py $acnt.$acnt.fof.vis-id.csv $acnt.nobody.fof.vis-id.csv >> $acnt.combine.fof.priv-bin-setting.csv

rm -rf $files
