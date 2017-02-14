#This script will create a decriptive statistics table for oj model for some variables
#######################################################################################

#Creating a subset of required number of columns from the dataset 
df <- unique(oj[c(7:17)]) 

#Generating a list of subset of columns 
Des_stats <- do.call(data.frame, 
             list(Average = apply(df, 2, mean),
                  Standard_Deviation = apply(df, 2, sd),
                  Median = apply(df, 2, median),
                  Minimum = apply(df, 2, min),
                  Maximum = apply(df, 2, max)))

print(Des_stats)

#Correlation 
corr = apply(df,2,quantile,probs=c(0.05,.1,.5,.95,.99))
print(corr)
corr1 <- cor(df,use = "complete.obs",method="kendall")
print(round(corr1,digits=3))

pairs(df,panel = points,main="Correlation plot among variables of dataset")
######################################################################################
