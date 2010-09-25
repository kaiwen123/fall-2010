package project2;

import java.io.IOException;
import java.util.*;

import org.apache.hadoop.fs.Path; 
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.util.*;

public class IndexReverser {
public static class Map extends MapReduceBase implements<LongWritable, Text, Text, IntWritable> {
private final static IntWritable one new IntWritable(1);
private Text word = new Text(); 

public void map()
}
}
