source ~/.bash_profile
javac -classpath /usr/local/hadoop/hadoop-0.20.1-core.jar  -d link-reverser/ LinkReverser.java
jar -cvf link-reverser.jar -C link-reverser/ .
hadoop fs -rmr /user/w234sxg/project2/output
hadoop jar link-reverser.jar LinkReverser project2/input project2/output
rm -rf result.txt
hadoop fs -get project2/output/part-00000 result.txt
