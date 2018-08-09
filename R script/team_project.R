rm(list=ls())

library(InformationValue)
library(caret)

df <-read.csv("C:/Joey/Intro to Predictive Model/bank-additional/bank-additional-full.csv")

# Check if there is any missing value
apply(df, 2, function(x) any(is.na(x)))

# Change y (yes to 1, no to 0)
attach(df)
new_y<-ifelse(y=="no",0,1)
df<-cbind(df,new_y)

# Transform age into bins
age_bins <- cut(age, breaks=c(0,20, 40, 60, 80, 100))
df <- cbind(df, age_bins)


#######################
## Information Value ##
#######################

# Install the package

## library(devtools)
## install_github("selva86/InformationValue")

#Get all categorical variables
factor_vars <- names(df)[c(2:10,15,22)]

# Compute Information Value
df_iv <- data.frame(VARS=factor_vars, InfmV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)),stringsAsFactors = F)  
for (i in factor_vars){
  df_iv[df_iv$VARS == i, "InfmV"] <- 
    IV(X=df[,i], Y=new_y)
  df_iv[df_iv$VARS == i, "STRENGTH"] <- 
    attr(IV(X=df[,i], Y=new_y), "howgood")
}

# Sort
df_iv <- df_iv[order(-df_iv$InfmV), ]
df_iv



#########################
## Logistic Regression ##
#########################

# Logistic Regression (After IV Feature Selection)

x1 <- df_iv$VARS[c(1:5)]
x2 <- names(df)[c(12:14,16:20)]
x <- append(x1,x2)
func <- paste("new_y~",paste(x,collapse = "+"))

log.fit <- glm(func, data=df, family=binomial)
summary(log.fit)


# 10-Fold CV

kcv = 10
iteration = 10

err_matrix <- matrix(0,kcv,iteration)

for (i in 1:iteration) {
  
  n0 = round(nrow(df)/kcv,0)
  set <- 1:nrow(df)
  used <- NULL
  
  for (j in 1:kcv) {
  
  if(n0<length(set)){index = sample(set,n0)}
  if(n0>=length(set)){index=set}
  
  train <- df[index,]
  test <- df[-index,]
  
  fit <- glm(func, data=train, family=binomial)
  pred <- predict(fit,newdata=test,type='response')
  results <- ifelse(pred > 0.4,1,0)
  answers <- test$new_y
  
  err_matrix[j,i] <- mean(answers != results)

  used = union(used,index)
  set = (1:nrow(df))[-used]
  
  }
  cat("10-Fold CV No. ",i,'\n')
}

err_rate <- mean(apply(err_matrix,2,mean))

table(new_y[-index],pred>0.4)


# EDA Plot

library(dplyr)
library(ggplot2)
library(reshape2)


is.fact <- sapply(df, is.factor)
factors.raw <- df[, is.fact]
mdata <- melt(factors.raw, id=c("y"))
k <- summarise(group_by(mdata,variable,value,y),count =n())
uniq <- unique(k$variable)

for (i in uniq){
  idx <- match(i,uniq)
  l = list()
  t <- subset(k,k$variable == i)
  assign(paste("k", i, sep = ""), data.frame(t))
  p <- ggplot(t, aes(factor(value), count, fill = y)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_brewer(palette = "Set1") + ggtitle(i) 
  assign(paste("p", idx, sep = ""), p) # need to check the syntax
}

require(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol=3)


t1 <- subset(k,(k$variable == 'education' & k$value != 'unknown'))
plot1 <- ggplot(t1, aes(factor(value), count, fill = y)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") + ggtitle('Education')
plot1

t2 <- subset(k,k$variable == 'month')
plot2 <- ggplot(t2, aes(factor(value), count, fill = y)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") + ggtitle('Month')
plot2

t3 <- subset(k,k$variable == 'poutcome')
plot3 <- ggplot(t3, aes(factor(value), count, fill = y)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") + ggtitle('Outcome of the Previous Marketing Campaign')
plot3


plot4 = plot(y,col=c(2,4))














