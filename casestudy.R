
## Step 1  - Read in Data
data=read.csv("casestudydata.csv")
names(data)



class(data)
summary(data)
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status



## Step 2  - Missing Data
summary(data_in)
dim(data_in)
?na.omit
data_in=na.omit(data_in)
dim(data_in)

## Step 3 and 4 - Correlation
cor(data_in)
summary(data_in)
data_new=model.matrix(~-1+Racegrp+CareSource,data=data_in)
summary(data_new)
data_in=data_in[,-c(4,8)]
data_in=cbind(data_in,data_new)
cor(data_in)
names(data_in)
data_in=data_in[,-33]
cor(data_in)
  ## any highly correlated - large relation to PCA


## Step 5 - Run a Regression
model=lm(CKD~.,data=data_in)
summary(model)

summary(predict(model))


## Step 6 - Screening tool

#  GET CREATIVE :))))  :)) ha ha 

## Hint:  Do you have Diabetes?  (Yes = c points or No = d points)

## PCA intro

#How do we plot all 33 variables?

dim(data_in)
install.packages('pca3d')
library(pca3d)

pca <- prcomp( data_in[,-32], scale.= TRUE )
pca3d( pca, group= data_in[,32] )

