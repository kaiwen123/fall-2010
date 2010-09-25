/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//package proj2;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
//import java.util.Hashtable;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
//import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileInputFormat;
import org.apache.hadoop.mapred.FileOutputFormat;
import org.apache.hadoop.mapred.JobClient;
import org.apache.hadoop.mapred.JobConf;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

/**
 * The purpose of this class is to reverse the link of a given file.
 * The input file is of this format: id \t word1 word2 ... wordn
 * The output should be of the format as: wordx id1:pos1 id2:pos2
 */
@SuppressWarnings("deprecation")
public class LinkReverser extends Configured implements Tool {  
    /**
     * Counts the words in each line.
     * For each line of input, break the line into words and emit them as
     * (<b>word</b>, <b>1</b>).
     */
    public static class MapClass extends MapReduceBase
	implements Mapper<LongWritable, Text, Text, Text> {
	private Text words = new Text("");
	private Text index = new Text(""); // index like: 1:19
    
	public void map(LongWritable key, Text value, 
			OutputCollector<Text, Text> output, 
			Reporter reporter) throws IOException {
	    String line = value.toString();
	    String x[] = line.split("\t", 2);
	    String token = ""; 	// words
	    String idx = x[0];
	    int offset = -1; 

	    StringTokenizer itr = new StringTokenizer(x[1]);
	    while (itr.hasMoreTokens()) {
		offset += token.length() + 1; 
		token = itr.nextToken(); 
		words.set(token.toLowerCase().replaceAll("[,.]", ""));
		index.set(idx + ":" + Integer.toString(offset)); 
		//System.out.println(words + " " + index); 
		output.collect(words, index);
	    }
	}
    }
  
    /**
     * A reducer class that will be responsible for collecting the position 
     * lists for each words and put them into array like storage. 
     */
    public static class Reduce extends MapReduceBase
	implements Reducer<Text, Text, Text, Text> {

	// A hashmap object to store the results. 
	// First String variable will be key; 
	// Second String variable will be value. 
	String index = ""; 
	HashMap<String, String> indexList = new HashMap<String, String>();
	public void reduce(Text key, Iterator<Text> values,
			   OutputCollector<Text, Text> output, 
			   Reporter reporter) throws IOException {
	    while (values.hasNext()) {
		String tmp = ""; 
	    	if(indexList.containsKey(key.toString()))
		    tmp = indexList.get(key.toString()); 
		indexList.put(key.toString(), tmp + " " + values.next().toString());
	    } 
	    output.collect(key, new Text(indexList.get(key.toString())));	
	}
    }
    static int printUsage() {
	System.out.println("indexreverser [-m <maps>] [-r <reduces>] <input> <output>");
	ToolRunner.printGenericCommandUsage(System.out);
	return -1;
    }

    /**
     * The main driver for word count map/reduce program.
     * Invoke this method to submit the map/reduce job.
     * @throws IOException When there is communication problems with the 
     *                     job tracker.
     */
    public int run(String[] args) throws Exception {
	JobConf conf = new JobConf(getConf(), LinkReverser.class);
	conf.setJobName("indexreverser");
 
	conf.setOutputKeyClass(Text.class);
	conf.setOutputValueClass(Text.class);
    
	conf.setMapperClass(MapClass.class);        
	conf.setCombinerClass(Reduce.class);
	conf.setReducerClass(Reduce.class);
    
	List<String> other_args = new ArrayList<String>();
	for(int i=0; i < args.length; ++i) {
	    try {
		if ("-m".equals(args[i])) {
		    conf.setNumMapTasks(Integer.parseInt(args[++i]));
		} else if ("-r".equals(args[i])) {
		    conf.setNumReduceTasks(Integer.parseInt(args[++i]));
		} else {
		    other_args.add(args[i]);
		}
	    } catch (NumberFormatException except) {
		System.out.println("ERROR: Integer expected instead of " + args[i]);
		return printUsage();
	    } catch (ArrayIndexOutOfBoundsException except) {
		System.out.println("ERROR: Required parameter missing from " +
				   args[i-1]);
		return printUsage();
	    }
	}
	// Make sure there are exactly 2 parameters left.
	if (other_args.size() != 2) {
	    System.out.println("ERROR: Wrong number of parameters: " +
			       other_args.size() + " instead of 2.");
	    return printUsage();
	}
	FileInputFormat.setInputPaths(conf, other_args.get(0));
	FileOutputFormat.setOutputPath(conf, new Path(other_args.get(1)));
        
	JobClient.runJob(conf);
	return 0;
    }
  
  
    public static void main(String[] args) throws Exception {
	int res = ToolRunner.run(new Configuration(), new LinkReverser(), args);
	System.exit(res);
    }

}
