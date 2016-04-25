# Introduction #
Usually, data from experiments are not suitable for doing data mining tasks. Because the raw data may contain out-of-range-values, impossible data combination or missing value etc. Analyzing data without being carefully screened can produce misleading resules. Thus, the raw data needs to pre-processed before doing data mining. And often-times, this step can take considerable amount of processing time.

Data pre-processing includes cleaning, normalization, transformation, feature selection and extraction etc. The product of data pre-processing is the final training data set.

In my project one, we do data transformation and/or normalization by means of equal-width binning and entropy based binning.

# Equi-Width Binning #
Equi-Width binning is the process of discretizing (continuous) feature data by dividing the data into a number of equal width value. For example, we have values 0 1 2 10 12 5 20 22 8 and we want to discretize it to 4 equally separated width data. Then the width of bin will be (20 - 0) / 4 = 5, and we have bins (-, 5], (5, 10], (10, 15], (15, +). And the discretized data is : a a a b c a d d b.

# Entropy Based Data Binning #
Entropy based data binning tries to split the (continuous) data into discretized data so that maximum information gain can be achieved. Information gain can be achieved by calculating subtracting information split from the original information entropy. E.g.
G(info\_gain) = E(original) - S(split). In this project we need to try all the possible split and find the one with the highest information gain.
Note: the processing of testing all the splits is computational extensive, so it is advisable to sort the feature(gain) data first and do one pass processing.

# Project README #
**FILES**
```
- main.cpp
Main project file to test the implementation.
- GeneFeatureItem.{h, cpp}
Files to define and implement a feature along with the class data for a gene.
- GeneFeatureBins.{h,cpp}
Files to define and implement a bin object.
- GeneFeatureData.{h, cpp}
Files to define and implement the data for a particular feature.
- GeneDataSet.{h,cpp}
Files to store the feature set for genes.
- defs.h
File used to define some macros.
- README
This file.
- Makefile
The project management file.
```
**HOWTO**

Simply type make in the current working directory, which will generate
the executable binningexe file.
Run the executable by ./binningexe
Provide input according to the program prompt.
Check the result in the current working directory.

**TODO**

- Singular design pattern for GeneDataSet class.
- Move the function of loading data into GeneDataSet class.
This is more complied with the OOP design mechanism.

# Project Execution Sample Results #
Data preprocessing - Sample output: four bins for equi-width and two bins for entropy

**ewidth.bins**
```
G0: [-inf,3719.1)4, [3719.1,7438.2)38, [7438.2,11157.3)12, [11157.3,+inf)8, 
G1: [-inf,2538.07)8, [2538.07,5076.14)26, [5076.14,7614.21)20, [7614.21,+inf)8, 
G2: [-inf,2151.26)8, [2151.26,4302.52)30, [4302.52,6453.78)17, [6453.78,+inf)7, 
G3: [-inf,2812.17)17, [2812.17,5624.34)35, [5624.34,8436.51)7, [8436.51,+inf)3, 
G4: [-inf,2023.47)21, [2023.47,4046.94)30, [4046.94,6070.41)10, [6070.41,+inf)1,
```
**ewidth.data**
```
c,c,b,b,a,negative
c,c,c,b,a,positive
b,c,c,b,a,negative
b,d,c,b,a,positive
a,b,b,b,b,negative
```
**entropy.bins**
```
G248: [-inf,1765.18)44, [1765.18,+inf)18, 
G492: [-inf,387.243)37, [387.243,+inf)25, 
G764: [-inf,935.204)50, [935.204,+inf)12, 
G1670: [-inf,62.7375)14, [62.7375,+inf)48, 
G1771: [-inf,87.71)33, [87.71,+inf)29,
```
**entropy.data**
```
a,b,a,b,b,negative
b,b,b,a,a,positive
a,a,a,b,a,negative
a,b,a,a,a,positive
a,a,a,b,b,negative
```