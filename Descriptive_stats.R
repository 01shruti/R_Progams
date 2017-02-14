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

reg1 <- lm(df$EDUC~df$HVAL150)
par(cex=0.8)
plot(df$EDUC,df$HVAL150)
abline(reg1)

reg2 <- lm(df$AGE60~df$EDUC)
par(cex=0.9)
plot(df$AGE60,df$EDUC)
abline(reg2)

######################################################################################
hist(df$AGE60, xlab = "%age of population aged 60 or older",main="Histogram of AGE60")
######################################################################################

library(bayesm)
store <- levels(as.factor(oj$store))
n <- length(unique(store))
ldata = NULL

mcmc = list(R=2000,keep=15)
out = rhierMnlRwMixture(Data = list(p=4,lgtdata = df),Prior=list(ncomp=1),Mcmc= mcmc)
