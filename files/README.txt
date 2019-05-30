Datasets used in the experiments section are given in the Datasets.R code. 
You can run the algorithm  by only running the Dataset.R code.
Make sure to set your working directory as the folder of Supplementary Materials. 

If you want to apply the algorithm in other datasets, 
please run the following code and then apply the discoverpolarity algorithm: 
source("Polarity Discovery Code.R")


Inputs of the Algorithm:

1. causes:	Cause variables must be given as data.frame() including the cause variable names as the column names. Data must free of missing values. The number of cause variables to be used (n) is not limited. However, if n is beyond 4-5, the chance of obtaining a unique solution significantly decreases. For too many causal variables, significantly more data points are needed to cover the causal domain.

2. effect:	Corresponding effect variable as a vector. The vector must be free of missing values.

3. threshold1:  The threshold to select first candidate indexes for each cause variable to set them to zero. The indexes that have a change less than threshold1% of their limits are considered as the first candidate indexes. The default value is 15%.

4. threshold2:	Threshold2 is used to select indexes from the first candidate indexes list to set their value as 0. In the indexes, the causal variables that have the value of (percentage change in the candidate cause the variable/sum of all the percentage changes in all the cause variables at that difference) smaller than threshold2 are set as 0. The default threshold2 is the value of threshold1. 

5. threshold3:	The threshold to select indexes for the effect variable to set the effect variable as zero. The default value is set as the value of threshold1.

6. varImp:	Variable Importance is used specifically in the second phase. It must be given as a vector having importance value for each cause variable with the same order as they are given in the causes data frame. The minimum value must be set as 1 and the others must be specified relative to that variable. In the default setting, all the variables take the varImp value as 1.

7.limits:	Limits show the range of cause variables where the variables significantly affect Y variable. If a variable always has a significant impact on the Y variable, the limit can be set as the limits of the observed data of that variable. However, the varImp must be chosen relative to the limits.



Outputs of the Algorithm:

If the first phase of the algorithm finds a unique solution, it returns the solution (All1) and the eliminated possibilities table (EliminatedOptions1) with the corresponding number of differences used to eliminate the corners (NoofObservedDifferences). The idea behind returning the eliminated possibilities is to show how many point differences we had to decide that this corner must be eliminated. In some cases, the researcher may want to check the differences and the points used to get these differences when a possibility is eliminated with very small evidences (number of differences). 

If the algorithm goes into the second phase, it returns results of both first and second phases (All1, EliminatedOptions1, All2, EliminatedOptions2). Therefore, the modeler can see the results for before and after applying the second phase. The solution (All2), sometimes multiple ones, and eliminated possibilities table (EliminatedOptions2) are given as the outputs of the second phases just like the first one. 


