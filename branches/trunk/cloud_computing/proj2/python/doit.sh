hadoop fs -rmr project2/output
hadoop jar /usr/local/hadoop/hadoop-streaming.jar -mapper "python mapper.py" -reducer "python reducer.py" -file mapper.py -file reducer.py -input project2/input -output project2/output
rm -rf result.txt
hadoop fs -get project2/output/part-00000 result.txt
