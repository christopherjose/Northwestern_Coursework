# Data Preparation --------------------------------------------

rm(list=ls()) #remove all objects
dev.off() #clear all plots
library(BBmisc)
library(pastecs)
library(bnclassify)
library(PROC)
library(ROCR)
library(plotROC)
library(randomForest)
library(boot)
library('plyr')
library(tictoc)
library(reshape2)
library("ggcorrplot")
library(caret)
library(Hmisc)
library(ParamHelpers)
library(gbm)
library(pls)
library(leaps)
library(glmnet)
library(xlsx)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(stargazer)
library(dplyr)
library(plyr)
library(scales)
library(MASS)
library(tidyr)


#DATA PREPROCESSING
options(scipen=999) #remove scientific notation
original_data<-readRDS('/Users/Work/Desktop/Work/Projects/Northwestern/Credit Risk Modeling Capstone/credit_card_default.RData')
df<-readRDS('/Users/Work/Desktop/Work/Projects/Northwestern/Credit Risk Modeling Capstone/credit_card_default.RData')

str(df)
describe(df)  #data quality check
table(df['data.group']) #train, test, validation split count
df<-df[,-which((names(df) %in% c('ID')))] #drop ID
df$EDUCATION[df$EDUCATION == 0] <- 6 #Remap variable values not in line w/ data dictionary
df$MARRIAGE[df$MARRIAGE == 0] <- 3
t(colSums(is.na(df))) #missing values check

#Print number of infinite values in dataframe
temp<-0
for (j in 1:length(names(df))){
  temp1<-any(sapply(df[names(df)[j]],is.infinite))
  temp<-temp+temp1
}
print(temp) 
attach(df)

# Feature Engineering --------------------------------------------
#Utilization is % of original credit that still needs to be paid off (balance / original balance)
df$UTILIZATION_1 <- BILL_AMT1  /  LIMIT_BAL #September
df$UTILIZATION_2 <- BILL_AMT2  /  LIMIT_BAL
df$UTILIZATION_3 <- BILL_AMT3  /  LIMIT_BAL
df$UTILIZATION_4 <- BILL_AMT4  /  LIMIT_BAL
df$UTILIZATION_5 <- BILL_AMT5  /  LIMIT_BAL
df$UTILIZATION_6 <- BILL_AMT6  /  LIMIT_BAL # April
detach(df)
attach(df) #attach UTILIZATION variables

#Balance Velocity
#Make ratio 1 if prior or both utilizations are 0 (i don't think i need both here haha)
#Since this means later utilization makes up 100% of prior utilization so we want 1, not 0
df$BALANCE_VELOCITY_6 <- ifelse(UTILIZATION_6==0,1,
                                ifelse(BILL_AMT6<0, 1,
                                UTILIZATION_5 / UTILIZATION_6)) # (May / April)
df$BALANCE_VELOCITY_5 <- ifelse(UTILIZATION_5==0,1,
                                ifelse(BILL_AMT5<0, 1,
                                UTILIZATION_4 / UTILIZATION_5))
df$BALANCE_VELOCITY_4 <- ifelse(UTILIZATION_4==0,1,
                                ifelse(BILL_AMT4<0, 1,
                                UTILIZATION_3 / UTILIZATION_4))
df$BALANCE_VELOCITY_3 <- ifelse(UTILIZATION_3==0,1,
                                ifelse(BILL_AMT3<0, 1,
                                UTILIZATION_2 / UTILIZATION_3))
df$BALANCE_VELOCITY_2 <- ifelse(UTILIZATION_2==0,1,
                                ifelse(BILL_AMT2<0,1,
                                UTILIZATION_1 / UTILIZATION_2))  #(September / August)

df$BALANCE_VELOCITY_6<-ifelse(UTILIZATION_6==0,1,
       ifelse(BILL_AMT5<0 & BILL_AMT6<0 & BILL_AMT5<BILL_AMT6, 
           -1*BILL_AMT5/BILL_AMT6,
       ifelse(BILL_AMT5<0 & BILL_AMT6<0 & BILL_AMT5>BILL_AMT6,
             BILL_AMT6/BILL_AMT5, 
       ifelse(BILL_AMT6<0, 1,
       BILL_AMT5/BILL_AMT6)
       )))

df$BALANCE_VELOCITY_5<-ifelse(UTILIZATION_5==0,1,
                              ifelse(BILL_AMT4<0 & BILL_AMT5<0 & BILL_AMT4<BILL_AMT5, 
                                     -1*BILL_AMT4/BILL_AMT5,
                                     ifelse(BILL_AMT4<0 & BILL_AMT5<0 & BILL_AMT4>BILL_AMT5,
                                            BILL_AMT5/BILL_AMT4, 
                                            ifelse(BILL_AMT5<0, 1,
                                                   BILL_AMT4/BILL_AMT5)
                                     )))

df$BALANCE_VELOCITY_4<-ifelse(UTILIZATION_4==0,1,
                              ifelse(BILL_AMT3<0 & BILL_AMT4<0 & BILL_AMT3<BILL_AMT4, 
                                     -1*BILL_AMT3/BILL_AMT4,
                                     ifelse(BILL_AMT3<0 & BILL_AMT4<0 & BILL_AMT3>BILL_AMT4,
                                            BILL_AMT4/BILL_AMT3, 
                                            ifelse(BILL_AMT4<0, 1,
                                                   BILL_AMT3/BILL_AMT4)
                                     )))

df$BALANCE_VELOCITY_3<-ifelse(UTILIZATION_3==0,1,
                              ifelse(BILL_AMT2<0 & BILL_AMT3<0 & BILL_AMT2<BILL_AMT3, 
                                     -1*BILL_AMT2/BILL_AMT3,
                                     ifelse(BILL_AMT2<0 & BILL_AMT3<0 & BILL_AMT2>BILL_AMT3,
                                            BILL_AMT3/BILL_AMT2, 
                                            ifelse(BILL_AMT3<0, 1,
                                                   BILL_AMT2/BILL_AMT3)
                                     )))

df$BALANCE_VELOCITY_2<-ifelse(UTILIZATION_2==0,1,
                              ifelse(BILL_AMT1<0 & BILL_AMT2<0 & BILL_AMT1<BILL_AMT2, 
                                     -1*BILL_AMT1/BILL_AMT2,
                                     ifelse(BILL_AMT1<0 & BILL_AMT2<0 & BILL_AMT1>BILL_AMT2,
                                            BILL_AMT2/BILL_AMT1, 
                                            ifelse(BILL_AMT2<0, 1,
                                                   BILL_AMT1/BILL_AMT2)
                                     )))
detach(df)
attach(df)

#payment ratio measures % of balance paid during a given month
df$PAYMENT_RATIO_2 <- ifelse((BILL_AMT2==0), 1, PAY_AMT1 / BILL_AMT2) #AUGUST
df$PAYMENT_RATIO_3 <- ifelse((BILL_AMT3==0), 1, PAY_AMT2 / BILL_AMT3) #JULY
df$PAYMENT_RATIO_4 <- ifelse((BILL_AMT4==0), 1, PAY_AMT3 / BILL_AMT4) #JUNE
df$PAYMENT_RATIO_5 <- ifelse((BILL_AMT5==0), 1, PAY_AMT4 / BILL_AMT5) #MAY
df$PAYMENT_RATIO_6 <- ifelse((BILL_AMT6==0), 1, PAY_AMT5 / BILL_AMT6) #APRIL  (PAY_AMT prior month's pay)

#If pay_ratio <=0, make 1 (since payment at least 100% of balance)
df$PAYMENT_RATIO_2[df$BILL_AMT2 <= 0] <- 1
df$PAYMENT_RATIO_3[df$BILL_AMT3 <= 0] <- 1
df$PAYMENT_RATIO_4[df$BILL_AMT4 <= 0] <- 1
df$PAYMENT_RATIO_5[df$BILL_AMT5 <= 0] <- 1
df$PAYMENT_RATIO_6[df$BILL_AMT6 <= 0] <- 1


# Engineered Feature Cleansing --------------------------------------------
#Checking above BALANCE_VELOCITY_6 code
temp<-df[,c('BILL_AMT5', 'BILL_AMT6', 'BALANCE_VELOCITY_6')]
temp1<-subset(temp,BILL_AMT5<0 & BILL_AMT6<0 & BILL_AMT5<BILL_AMT6)
temp1<-subset(temp,BILL_AMT5<0 & BILL_AMT6<0 & BILL_AMT5>BILL_AMT6)
temp1<-subset(temp,BILL_AMT6<0)

#Checking above BALANCE_VELOCITY_5 code
temp<-df[,c('BILL_AMT4', 'BILL_AMT5', 'BALANCE_VELOCITY_5')]
temp1<-subset(temp,BILL_AMT4<0 & BILL_AMT5<0 & BILL_AMT4<BILL_AMT5)
temp1<-subset(temp,BILL_AMT4<0 & BILL_AMT5<0 & BILL_AMT4>BILL_AMT5)
temp1<-subset(temp,BILL_AMT5<0)

#Checking above BALANCE_VELOCITY_4 code
temp<-df[,c('BILL_AMT3', 'BILL_AMT4', 'BALANCE_VELOCITY_4')]
temp1<-subset(temp,BILL_AMT3<0 & BILL_AMT4<0 & BILL_AMT3<BILL_AMT4)
temp1<-subset(temp,BILL_AMT3<0 & BILL_AMT4<0 & BILL_AMT3>BILL_AMT4)
temp1<-subset(temp,BILL_AMT4<0)

#Checking above BALANCE_VELOCITY_3 code
temp<-df[,c('BILL_AMT2', 'BILL_AMT3', 'BALANCE_VELOCITY_3')]
temp1<-subset(temp,BILL_AMT2<0 & BILL_AMT3<0 & BILL_AMT2<BILL_AMT3)
temp1<-subset(temp,BILL_AMT2<0 & BILL_AMT3<0 & BILL_AMT2>BILL_AMT3)
temp1<-subset(temp,BILL_AMT3<0)

#Checking above BALANCE_VELOCITY_2 code
temp<-df[,c('BILL_AMT1', 'BILL_AMT2', 'BALANCE_VELOCITY_2')]
temp1<-subset(temp,BILL_AMT1<0 & BILL_AMT2<0 & BILL_AMT1<BILL_AMT2)
temp1<-subset(temp,BILL_AMT1<0 & BILL_AMT2<0 & BILL_AMT1>BILL_AMT2)
temp1<-subset(temp,BILL_AMT2<0)


#DATA CLEANSING - PAYMENT RATIO
temp<-df[,which((names(df) %in% c(
  'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6',
  'PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6',
  'PAYMENT_RATIO_2','PAYMENT_RATIO_3','PAYMENT_RATIO_4','PAYMENT_RATIO_5','PAYMENT_RATIO_6')))]

#Payment ratio scenarios, payment ratio 2 is infinite (>0, 0) 
temp1<-subset(temp, BILL_AMT2==0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')] #2506 cases
temp<-subset(temp1, PAY_AMT1>0) #38 cases
temp<-subset(temp1, PAY_AMT1<0) #0 cases

#payment ratio 2 is negative (<0, 0) ---> there are no negative PAY_AMT_j's
temp1<-subset(temp, PAY_AMT1<0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')] #0 cases

#payment ratio 2 is negative (>0, <0) #since pay_amt is always >=0, this means pay is always greater, and so make pay ratio = 1  
temp1<-subset(temp, BILL_AMT2<0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')] #669 cases

#payment ratio >1 (pay>bal, bal>0)
temp1<-subset(temp, PAY_AMT1>BILL_AMT2)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')] #3464 cases
temp1<-subset(temp1,BILL_AMT2>0) #2757 cases
temp2<-subset(original_data,ID=='10281')
temp3<-df[,c('BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6',
  'PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6')]

#Sometimes bill_amt goes from >0 -> 0 -> >0
#Check rows where variable has infinite value (its because balance 0 and payment nonzero)
temp<-temp[temp[,'PAYMENT_RATIO_4']==Inf,]
temp<-subset(df, PAY_AMT6<0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')] #0 obs
temp2<-subset(original_data,ID=='23260') #check individual

#no need to account for payment <0 since there are none #cases of PAY_AMT1 > BILL_AMT2
temp<-subset(df, PAY_AMT1<0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')]
temp<-subset(df, PAY_AMT1>BILL_AMT2)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')]
temp2<-subset(df, BILL_AMT2==0)[,c('PAY_AMT1','BILL_AMT2','PAYMENT_RATIO_2')]  #cases of (pay>bal and bal = 0)

#DATA CLEANSING - BALANCE VELOCITY
temp<-df[,which((names(df) %in% c(
  'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6',
  'BALANCE_VELOCITY_2','BALANCE_VELOCITY_3','BALANCE_VELOCITY_4','BALANCE_VELOCITY_5',
  'BALANCE_VELOCITY_6')))]
temp<-subset(temp,BALANCE_VELOCITY_6>1)[,c('BILL_AMT1','BILL_AMT2','BALANCE_VELOCITY_2')] #0 obs
temp2<-subset(original_data,ID=='23260') #check individual

#DATA CLEANSING - UTILIZATION J
temp<-subset(df)[,c('UTILIZATION_1','BILL_AMT1','LIMIT_BAL')]

# Variable Type Bucketing and Dataframe Prep--------------------------------------------
#Bucketing Variables by Variable Type
ordinal     <- c('EDUCATION','PAY_0','PAY_2','PAY_3','PAY_4','PAY_5','PAY_6') #'BINNED_AGE', 
nominal     <- c('MARRIAGE','SEX')
categorical <- c(ordinal,nominal)
other       <- c('u','train','test','validate' ,'data.group')
continuous  <- c('LIMIT_BAL', 'AGE', 
                'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6',
                'PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6',
                'PAYMENT_RATIO_2','PAYMENT_RATIO_3','PAYMENT_RATIO_4','PAYMENT_RATIO_5','PAYMENT_RATIO_6',
                'BALANCE_VELOCITY_2','BALANCE_VELOCITY_3','BALANCE_VELOCITY_4','BALANCE_VELOCITY_5','BALANCE_VELOCITY_6',
                'UTILIZATION_1','UTILIZATION_2','UTILIZATION_3','UTILIZATION_4','UTILIZATION_5','UTILIZATION_6')
engineered_features <- c('PAYMENT_RATIO_2','PAYMENT_RATIO_3','PAYMENT_RATIO_4','PAYMENT_RATIO_5','PAYMENT_RATIO_6',
                         'BALANCE_VELOCITY_2','BALANCE_VELOCITY_3','BALANCE_VELOCITY_4','BALANCE_VELOCITY_5','BALANCE_VELOCITY_6',
                         'UTILIZATION_1','UTILIZATION_2','UTILIZATION_3','UTILIZATION_4','UTILIZATION_5','UTILIZATION_6')
X           <- c(continuous, ordinal, 'MARRIAGE.1','MARRIAGE.2', 'MARRIAGE.3','SEX.1','SEX.2')
y           <- c('DEFAULT')

df.other       <- df[other]
df.ordinal     <- data.frame(lapply(df[ordinal], as.numeric))
df.nominal     <- data.frame(lapply(df[nominal], factor)) #applies as.factor() to every column
df.categorical <- cbind(df.ordinal,df.nominal)
df.continuous  <- data.frame(lapply(df[continuous], as.numeric))
df.engineered  <- df[engineered_features]

df<-(cbind(df.categorical,df.continuous, #formatted version 
          DEFAULT=as.factor(df$DEFAULT),
          u=df$u,
          train=as.factor(df$train),
          test=as.factor(df$test),
          validate=as.factor(df$validate),
          data.group=as.factor(df$data.group))) 

#Did I capture all variables in the above groupings?
names(df[,-which((names(df) %in% c(ordinal,nominal,continuous)))])
detach(df)
attach(df)

#One-hot encoding of nominals for model-building-dataframe
df.nominal.dummies <- dummyVars(" ~ .", data = df.nominal)
df.nominal.dummies <- data.frame(predict(df.nominal.dummies, newdata = df.nominal))

df.model <- (cbind(df.ordinal,df.nominal.dummies,df.continuous,  
                   DEFAULT=as.factor(df$DEFAULT),
                   train=as.factor(df$train),
                   test=as.factor(df$test))) #removed validate col b/c don't need

df.val <- cbind(df.ordinal,df.nominal.dummies,df.continuous,  
                   DEFAULT=as.factor(df$DEFAULT),
                   validate=as.factor(df$validate)) #the way i coded this is redundant

df.traintest <- cbind(df.ordinal,df.nominal.dummies,df.continuous,  
                     DEFAULT=as.factor(df$DEFAULT),
                     validate=as.factor(df$validate)) #the way i coded this is redundant

nrow(y.test)
train <- subset(df.model, train ==1) 
test  <- subset(df.model, test ==1)
validate <- subset(df.val, validate ==1)
traintest <- subset(df.traintest,validate==0)

train <- train[,-which((names(train) %in% c('train','test')))] #remove train/test indicators
test  <- test[,-which((names(test) %in% c('train','test')))]
validate <- validate[,-which((names(validate) %in% c('validate')))]
traintest <- traintest[,-which((names(traintest) %in% c('validate')))]

X.train <- train[X]
y.train <- train[y]

X.test <- test[X]
y.test <- test[y]

X.validate <- validate[X]
y.validate <- validate[y]

X.traintest <- traintest[X]
y.traintest <- traintest[y]



# Exploratory Data Analysis --------------------------------------------

summary(df)
df.engineered.summary<-do.call(cbind, lapply(df.engineered, summary))
temp<-df.engineered.summary[,1:5]
temp1<-df.engineered.summary[,6:10]
temp2<-df.engineered.summary[,11:16]

file.name  <-'engineered feature.html'
out.path = '/Users/Work/Desktop/Credit Risk Modeling Capstone/stargazer_output/'
stargazer(temp2, type=c('html'), out=paste(out.path, file.name,sep=''), font.size = 'tiny', 
          #title=c('EDUCATION'),
          align=TRUE, digits = 3,digits.extra=2, initial.zero=TRUE, summary = FALSE)


#Feature Selection - Low Variance Filter 
'''Remove normalized vars with highest freq value being (98/2) 
times that of 2nd most frequent var, and unique samples making up 10% of all samples   '''
df.normalize<-as.data.frame(t(apply(sapply(df,as.numeric), 1, function(x) (x - min(x))/(max(x)-min(x)))))
str(df.normalize)
remove_cols<-nearZeroVar(df.normalize, freqCut = 98/2, uniqueCut = 10)  #returns col 6, 7
#removes vars with most prevalent value having freq freqCut that of 2nd most prevalent var
#and (unique values / total samples) x 100 must be below uniqueCut
remove_cols #c(6,7) PAY_5, PAY_6
nrow(subset(df.normalize, df.normalize$PAY_2==0)) #19885 zeros
nrow(subset(df.normalize, df.normalize$PAY_3==0)) #20344 zeros
nrow(subset(df.normalize, df.normalize$PAY_4==0)) #20768 zeros
nrow(subset(df.normalize, df.normalize$PAY_5==0)) #21251 zeros
nrow(subset(df.normalize, df.normalize$PAY_6==0)) #21747 zeros
var_filter<-setdiff(names(df),names(df[remove_cols])) #vars excluding those to remove
names(df[,var_filter])   #evidence that vars are removed  

#Feature Selection - Pairwise Correlations and Multicollinearity   
#Create random splits of train to mimic this procedure in CV (since im not doing in CV)
#Continuous Variables
n <- nrow(train)
train.shuffled <- train[sample(n), ]
train.split.1.indices <- 1:round(0.6 * n)
train.split.1 <- train.shuffled[train.split.1.indices, ]
train.split.2.indices <- (round(0.6 * n) + 1):(0.8*n)
train.split.2 <- train.shuffled[train.split.2.indices, ]
train.split.3.indices <- (round(0.8*n)+1):n
train.split.3 <- train.shuffled[train.split.3.indices,]

train.split.1 <- train.split.1[continuous]
train.split.2 <- train.split.2[continuous]
train.split.3 <- train.split.3[continuous]

#Correlation Check Pipeline (check all 3 train splits)
cormat<-cor(train.split.1, train.split.1, method = 'pearson', use= 'pairwise')
cormat.rm<-ifelse(as.matrix(cormat)==1.0,0,as.matrix(cormat))
cormat.rm<-ifelse(cormat.rm>=.6,cormat.rm,0)
cormat.rm.df<-data.frame(cormat.rm)
cormat.rm.df 
# BILL_AMT j and UTILIZATION j have multicollinearity in all 3 splits
names(df.continuous)
#Make Correlation Plot
cormat<-cor(df.continuous, df.continuous, method = 'pearson', use= 'pairwise')
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat)
ggcorrplot(cormat, hc.order = TRUE, type = "lower", outline.col = "white",
           tl.cex = 6) # make variable font size smaller

#Bill Amount - Correlations
cormat[c('BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'),
       c('BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6')]
#PAY_AMT - Correlations

#PAYMENT_RATIO_2 and 4 are over 70% correlated, so remove one that's more correlated with target
cormat[c('PAYMENT_RATIO_2','PAYMENT_RATIO_3','PAYMENT_RATIO_4','PAYMENT_RATIO_5',
'PAYMENT_RATIO_6'),c('PAYMENT_RATIO_2','PAYMENT_RATIO_3','PAYMENT_RATIO_4','PAYMENT_RATIO_5',
'PAYMENT_RATIO_6')]

#Automating Pairwise Correlation Check

glm <- glm(as.formula(paste("DEFAULT~", paste(continuous, collapse = " + "), sep = " " )),
           family=binomial(link='logit'),data=df)  
glm.pvalues<-summary(glm)$coefficients[,'Pr(>|z|)']
glm.pvalues

'''
for (j in 1:nrow(cormat.rm.df)) {
  for (k in 1:length(cormat.rm.df[j,]) {
    if (cormat.rm.df[j,k]>0) {
      cormat.rm.df[row.names(cormat.rm.df)[j],]
    }
  }
'''

#Correlation between continuous predictors and outcome
for (j in 1:ncol(df.continuous)) {
  print(aggregate(df.continuous[j], by=list(df$DEFAULT), 
                  FUN=mean, na.rm=TRUE))
}

for (j in 1:ncol(df.continuous)) {
  print(data.frame((aggregate(df.continuous[j], by=list(df$DEFAULT), 
                              FUN=mean, na.rm=TRUE))))
}



#Creating histograms for continuous predictors by outcome levels

  #Mode
  Mode <- function(x) {
   ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  #Side-by-side histogram of age by default
  ggplot(df,aes(x=AGE))+ 
  geom_histogram(aes(y = 100*..density..))+facet_grid(~DEFAULT) +theme_bw() +
  ylab('Density') +
  theme(plot.title=element_text(size=11)) +
  ggtitle('Distribution of AGE by DEFAULT')

  #Histogram of AGE by Default
  means<-aggregate(x = AGE, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = AGE, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') + #alpha used for filling the density
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
      linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of AGE by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    theme(text = element_text(size = 8),
        legend.key.size=unit(.8,"line"))
  
  aggregate(x = AGE, by = list(DEFAULT), FUN = Mode)
  
  
  #Histogram of LIMIT_BAL by Default
  means<-aggregate(x = LIMIT_BAL, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = LIMIT_BAL, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') + 
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
      linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of LIMIT_BAL by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits = c(0, 750000), labels=comma) +  #set x limit at 750000
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))
    #setting x limit causes scientific notation, so i must inlcude labels=comma (from library(scale))
  
  #Histogram of BILL_AMT1 by Default
  means<-aggregate(x = BILL_AMT1, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = BILL_AMT1, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') + #alpha used for filling the density
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
               linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of BILL_AMT1 by DEFAULT') +
    scale_y_continuous(expand = c(0, 0), limits = c(0,4000)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits = c(0, 400000), labels=comma) + #set x limit at 750000
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))
  
  aggregate(x = BILL_AMT1, by = list(DEFAULT), FUN = mean)

  #Histogram of PAY_AMT1 by Default
  means<-aggregate(x = PAY_AMT1, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = PAY_AMT1, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') +
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
               linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of PAY_AMT1 by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits = c(-500, 40000), labels=comma)  +
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))
  
  #PAY_AMT1 Metrics
  temp<-subset(df,DEFAULT==0)['PAY_AMT1']
  temp2<-subset(df,DEFAULT==1)['PAY_AMT1']
  summary(temp)  #max is 880k
  summary(temp2) #max is 300k
  means
  
  #Histogram of PAYMENT_RATIO_2 by Default
  means<-aggregate(x = PAYMENT_RATIO_2, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  boxplot(df$PAYMENT_RATIO_2)
  ggplot(df, aes(x = PAYMENT_RATIO_2, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') +
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
               linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of PAYMENT_RATIO_2 by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits=c(0,.8),labels=comma) +
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))
  
  
  #PAYMENT_RATIO_2 Metrics
  temp<-subset(df,DEFAULT==0)['PAYMENT_RATIO_2']
  temp2<-subset(df,DEFAULT==1)['PAYMENT_RATIO_2']
  summary(temp)  
  summary(temp2) 
  means
  

  #Histogram of BALANCE_VELOCITY_2 by Default
  means<-aggregate(x = BALANCE_VELOCITY_2, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = BALANCE_VELOCITY_2, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') +
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
               linetype = 'dashed') +
    ylab('Count') +
    ggtitle('Histogram of BALANCE_VELOCITY_2 by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits = c(0,3), labels=comma) +
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))
  
  #BALANCE_VELOCITY_2 Metrics
  temp<-subset(df,DEFAULT==0)['BALANCE_VELOCITY_2']
  temp2<-subset(df,DEFAULT==1)['BALANCE_VELOCITY_2']
  summary(temp)  
  summary(temp2) 
  means
  
  
  #Histogram of UTILIZATION_2 by Default
  means<-aggregate(x = UTILIZATION_2, by = list(DEFAULT), FUN = "mean")
  names(means) = c('DEFAULT','mean')
  means$DEFAULT<-as.factor(means$DEFAULT)
  ggplot(df, aes(x = UTILIZATION_2, fill = DEFAULT)) +
    geom_histogram(alpha = .8, position='identity') +
    geom_vline(data = means, aes(xintercept = mean, color = DEFAULT), size = 1, alpha = 1,
               linetype = 'dashed') +
    ylab('Count') + 
    ggtitle('Histogram of UTILIZATION_2 by DEFAULT') +
    scale_y_continuous(expand = c(0, 0)) + #so the plot doesn't show space below (x=0,y=0)
    scale_x_continuous(limits=c(0,1), labels=comma) +
    theme(text = element_text(size = 8),
    legend.key.size=unit(.8,"line"))

  #UTILIZATION_2 Metrics
  temp<-subset(df,DEFAULT==0)['UTILIZATION_2']
  temp2<-subset(df,DEFAULT==1)['UTILIZATION_2']
  summary(temp)  
  summary(temp2) 
  means


#Format data for report by placing in a dataframe and using stargazer to print the dataframe
for (j in 1:ncol(df.continuous)) {
  temp[j]<-list(data.frame((aggregate(df.continuous[j], by=list(df$DEFAULT),
  FUN=mean, na.rm=TRUE))))}

temp2<-list()
for (j in 1:length(temp)){
  temp2[[j]]<-t(temp[[j]])
  colnames(temp2[[j]])<-c(0,1)
  temp2[[j]]<-temp2[[j]][-1,]}

continuous_relationship_outcome<-do.call(rbind,temp2) #combines all dataframe list elements using rbind() function
row.names(continuous_relationship_outcome)<-names(df.continuous)
rm(temp)
rm(temp2)

#Printing Correlations as pretty tables
out.path = '/Users/Work/Desktop/Credit Risk Modeling Capstone/stargazer_output/'
file.name1<-'continuous_relationship_outcome1.html'
file.name2<-'continuous_relationship_outcome2.html'

stargazer(continuous_relationship_outcome[1:15,], type=c('html'), out=paste(out.path, file.name1,sep=''),
            title=c('Continuous Predictor Relationship with Outcome'),
            #title=c('Table XX: __'),
            align=TRUE, digits = 2,digits.extra=2, initial.zero=TRUE, summary = FALSE)

stargazer(continuous_relationship_outcome[16:30,], type=c('html'), out=paste(out.path, file.name2,sep=''),
          #title=c('Table XX: __'),
          align=TRUE, digits = 2,digits.extra=2, initial.zero=TRUE, summary = FALSE)



#Correlation between categorical predictors and outcome
#Below is failing
for (j in 1:ncol(df.categorical)) {
  table(df.categorical[,j],df$DEFAULT)
}

#Below works
#BINNED_AGE generates an error.  Maybe because it's a factor variable.
prop.table(with(df, table(df[,categorical[1]], df$DEFAULT)),margin=1) #education
prop.table(with(df, table(df[,categorical[3]], df$DEFAULT)),margin=1) #pay_0
prop.table(with(df, table(df[,categorical[4]], df$DEFAULT)),margin=1) #pay_2
prop.table(with(df, table(df[,categorical[5]], df$DEFAULT)),margin=1) #pay_3
prop.table(with(df, table(df[,categorical[6]], df$DEFAULT)),margin=1) #pay_4
prop.table(with(df, table(df[,categorical[7]], df$DEFAULT)),margin=1) #pay_5
prop.table(with(df, table(df[,categorical[8]], df$DEFAULT)),margin=1) #pay_6 
prop.table(with(df, table(df[,categorical[9]], df$DEFAULT)),margin=1) #marriage
prop.table(with(df, table(df[,categorical[10]],df$DEFAULT)),margin=1) #sex


#EDUCATION
temp<-subset(df, select=c(EDUCATION,DEFAULT))
temp<-data.frame(temp %>% group_by(EDUCATION) %>% count(DEFAULT))
temp<-group_by(temp, EDUCATION) %>% mutate(PERCENT = n/sum(n))
temp<-temp[,c('EDUCATION','DEFAULT','PERCENT')]

p <- ggplot() + geom_bar(alpha=.5,aes(y = PERCENT, x = EDUCATION, fill = DEFAULT), 
                         data = temp,stat="identity")
p +   ylab("DEFAULT") +
  theme(text = element_text(size = 8)) + #plot.title = element_text(size=10), # legend.position = c(.8,.8)) + 
  labs(fill='Default') + #face='bold'
  ggtitle('Distribution of DEFAULT by EDUCATION') +
  theme(text = element_text(size = 8),
  legend.key.size=unit(.8,"line"))

#PAY_0
temp<-subset(df, select=c(PAY_0,DEFAULT))
temp<-data.frame(temp %>% group_by(PAY_0) %>% count(DEFAULT))
temp<-group_by(temp, PAY_0) %>% mutate(PERCENT = n/sum(n))
temp<-temp[,c('PAY_0','DEFAULT','PERCENT')]

p <- ggplot() + geom_bar(aes(y = PERCENT, x = PAY_0, fill = DEFAULT),data = temp,stat="identity")
p +   ylab("DEFAULT") + labs(fill='Default') +
  ggtitle('Distribution of DEFAULT by PAY_0') +
  theme(text = element_text(size = 8),
  legend.key.size=unit(.8,"line"))

#MARRIAGE
temp<-subset(df, select=c(MARRIAGE,DEFAULT))
temp<-data.frame(temp %>% group_by(MARRIAGE) %>% count(DEFAULT))
temp<-group_by(temp, MARRIAGE) %>% mutate(PERCENT = n/sum(n))
temp<-temp[,c('MARRIAGE','DEFAULT','PERCENT')]

p <- ggplot() + geom_bar(aes(y = PERCENT, x = MARRIAGE, fill = DEFAULT),data = temp,stat="identity")
p +   ylab("DEFAULT") + labs(fill='Default')  +
  ggtitle('Distribution of DEFAULT by MARRIAGE') +
   theme(text = element_text(size = 8),
   legend.key.size=unit(.8,"line"))


#SEX
temp<-subset(df, select=c(SEX,DEFAULT))
temp<-data.frame(temp %>% group_by(SEX) %>% count(DEFAULT))
temp<-group_by(temp, SEX) %>% mutate(PERCENT = n/sum(n))
temp<-temp[,c('SEX','DEFAULT','PERCENT')]

p <- ggplot() + geom_bar(aes(y = PERCENT, x = SEX, fill = DEFAULT),data = temp,stat="identity")
p +   ylab("DEFAULT") + labs(fill='Default') +
  ggtitle('Distribution of DEFAULT by SEX') +
  theme(text = element_text(size = 8),
  legend.key.size=unit(.8,"line"))

#Histogram of numeric and ordinal predictor values
dim(df.continuous)
par(mfrow=c(5,7),mai = c(.13, .17, .13, 0.1), cex=.3)
#mai(bottom, left, top, right) <- controls margin spacing around each plot in inches
for (j in colnames(df.continuous)){
  hist(df.continuous[[j]], main=j)
}

dev.off()
par(mfrow=c(1,1), cex=.3) #plot for 1 object
options(scipen=999)

#boxplots are too wide
ggplot(df, aes(x = DEFAULT, y = LIMIT_BAL)) +
  geom_point() +
  geom_boxplot(alpha=0)
names(df)
df2<-df[,-which((names(df) %in% c("BINNED_AGE")))]
yy<-df$DEFAULT
Xx<-df[,-which((names(df) %in% c("DEFAULT","train","u","test","validate","data.group")))]

#Make a decision tree to assess variable importances
form<-as.formula(yy~Xx)
form<-as.formula(paste('DEFAULT',"~", paste(names(Xx), collapse = " + "), sep = " " ))
tree <- rpart(form, data=df,  control = 
        rpart.control("minsplit" = 1,minbucket=1, cp=0.001))
printcp(tree)


split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=10), collapse="\n")
  }
  labs
}
rpart.plot(tree,split.fun=split.fun,cex=1.3, extra=100)

'''
tree$
rpart.plot(tree,tweak=1) #tweak seems similar to cex
prp(tree, cex=2) #shows the lines but isnt in color
plot(tree,uniform=TRUE)
par(mfrow=c(1,1), cex=.3,oma=c(0,0,2,0)) #oma fixes the title being cutoff
options(scipen=999)
text(tree,pretty =4)
'''
plotcp(tree) #shows that PAY_j is the most predictive variable
temp<-data.frame(tree$variable.importance)
out.path = '/Users/Work/Desktop/Credit Risk Modeling Capstone/stargazer_output/'
file.name1<-'dt.html'
stargazer(head(temp), type=c('html'), out=paste(out.path, file.name1,sep=''),
          title=c('Variable Importance - Decision Tree'),
          align=TRUE, digits = 2,digits.extra=2, initial.zero=TRUE, summary = FALSE)



#EDA for Feature Selection (Dimensionality Reduction)
#Variances relative to min/median
data.frame(stat.desc(df))[c('min','median','max','std.dev'),]
sapply(df.categorical,table) #inspect categorical variances







# Feature Engineering from EDA ------------------------------

temp1<-data.frame(avg.utilization=
apply(train[c("UTILIZATION_1", "UTILIZATION_2", "UTILIZATION_3", 
"UTILIZATION_4", "UTILIZATION_5", "UTILIZATION_6")],1,mean))
names(train)
temp2<-data.frame(avg.bill=apply(train[c("BILL_AMT1", "BILL_AMT2", 
    "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")],1,mean))
train.2 <- train[,-which((names(train) %in% c("UTILIZATION_1", "UTILIZATION_2", 
  "UTILIZATION_3", "UTILIZATION_4", "UTILIZATION_5", "UTILIZATION_6")))]
train.2 <- train.2[,-which((names(train.2) %in% c("BILL_AMT1", "BILL_AMT2", 
    "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")))]




# Model Building  --------------------------------------------
detach(df)
attach(train)

# Random Forests

control <- trainControl(method="cv", number=3)
set.seed(1)
model.rf <- train(DEFAULT~., data=train, method="rf",  #train hyperparameter tunes and gives a model performance estimate via CV
                  trControl=control, tuneLength=2,na.action=na.pass,#na.action=na.omit -> omits rows w/ missings
                  search = 'random', ntree=50, metric='Accuracy') #RF doesn't require feature scaling
set.seed(1)
model.rf2<-randomForest(DEFAULT~., train,metric='Accuracy',mtry=42,ntree=50) 
#using train for this produced error: Error: The tuning parameter grid should have columns mtry
preds.rf<-data.frame(predict=predict(object=model.rf2, newdata=X.test))
preds.rf['actual']<-as.numeric(as.character(test$DEFAULT))
preds.rf['0']<-NULL
preds.rf['model']<-"Random Forests"
names(preds.rf)<-c('predict','actual', 'model')



# GBM

control <- trainControl(method="cv", number=3)
set.seed(1) #use on line immediately prior to code chunk with variable results to make results constant
model.gbm <- train(DEFAULT~., data=train, method="gbm",  
                   trControl=control, tuneLength=5,na.action=na.pass) 
set.seed(1)
model.gbm2 <- train(DEFAULT~., data=train,method='gbm',
                  trControl=trainControl(method="none"),
                  tuneGrid=
                    data.frame(shrinkage=model.gbm$bestTune$shrinkage,
                    n.minobsinnode=model.gbm$bestTune$n.minobsinnode,
                    n.trees=model.gbm$bestTune$n.trees,
                    interaction.depth=model.gbm$bestTune$interaction.depth))

preds.gbm<-data.frame(predict(object=model.gbm2, newdata=X.test,n.trees=model.gbm2$n.trees))
preds.gbm['actual']<-as.numeric(as.character(test$DEFAULT))
preds.gbm['0']<-NULL
preds.gbm['model']<-"GBM"
names(preds.gbm)<-c('predict','actual', 'model')

auc(preds.gbm$actual,preds.gbm$predict) #generates AUC
ggplot(preds.gbm, aes(d = actual, m = predict, color='b')) + geom_roc(n.cuts = 0) 

# KNN

set.seed(1)
model.knn<-train(DEFAULT~.,data=train,method='knn',
                 trControl = trainControl(method='cv',number=3),
                 preProcess=c('range')) #using data=temp works

set.seed(1)
model.knn2<-train(DEFAULT~.,data=train,method='knn',
                 trControl = trainControl(method='none'),
                 tuneGrid=data.frame(k=model.knn$bestTune$k),
                 preProcess=c('range'))

preds.knn<-data.frame(predict(object=model.knn2, newdata=X.test))
preds.knn['actual']<-as.numeric(as.character(test$DEFAULT))
preds.knn['0']<-NULL
preds.knn['model']<-"KNN"
names(preds.knn)<-c('predict','actual', 'model')




# Random Forest Feature Selection

#Perform feature selection to only use important features via RF (100 secs) #RF too slow so reduced trees: https://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r/37382
set.seed(1)
fselect.rf<-randomForest(y.train[[1]]~., X.train, prox=TRUE,importance=TRUE,ntree=50, mtry=10) #default mtry=10
fselect.rf.imp1 <- data.frame(importance(fselect.rf,type=1)) #type=1 MeanDecreaseAccuracy #type=2 MeanDecreaseGini
temp<-row.names(fselect.rf.imp1) #assign row names
fselect.rf.imp1 <- data.frame(fselect.rf.imp1[order(-fselect.rf.imp1$MeanDecreaseAccuracy),],
                              row.names=temp) #order ascending
names(fselect.rf.imp1) <- 'MeanDecreaseAccuracy' #assign column name
fselect.rf.top15<-row.names(head(fselect.rf.imp1,15))

train.log <- train[c(fselect.rf.top15,'DEFAULT')]
test.log <- test[c(fselect.rf.top15)] 
validate.log <- validate[c(fselect.rf.top15)]


# Automated Variable Selection - Logistic Regression

k<-5 #These methods are not recommended according to https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection
set.seed(1) #StepAIC is only recommended for model prediction purposes
model.logit <- glm(DEFAULT ~ ., data = train.log, family = "binomial")

# Stepwise Selection of Logistic Regression

model.logit.null <-glm(DEFAULT ~ 1, data = train.log, family = "binomial")
model.logit.formula<-as.formula(paste("~", paste(fselect.rf.top15, collapse = " + "), sep = " " ) )
set.seed(1)
model.logit.step <- stepAIC(model.logit.null,scope = model.logit.formula, direction = 'both')
preds.logit.step=data.frame(predict(object=model.logit.step, newdata=test.log, type='response')) #response gives probs for class: glm


out.path = '/Users/Work/Desktop/Credit Risk Modeling Capstone/stargazer_output/'
file.name  <-'logit_model.html'
stargazer(as.data.frame(rbind(summary(model.logit.step)$coefficients)), 
          type=c('html'), out=paste(out.path, file.name,sep=''),
          title=c('Stepwise Logistic'),
          align=TRUE, digits = 3,digits.extra=2, initial.zero=TRUE, summary = FALSE)

(exp(-0.000002803)-1)*100000 #for a 100,000 increase in LIMIT_BAL
exp(-0.000002803*100000)
exp(0.000004816*100000)-1
summary(model.logit.step)


preds.logit.step['actual']<-as.numeric(as.character(test$DEFAULT))
preds.logit.step['0']<-NULL
preds.logit.step['model']<-"Logit"
names(preds.logit.step)<-c('predict','actual', 'model')
roc(DEFAULT ~ preds.logit.step, data = test.log)

library("ROCR")
perf1 <- prediction(preds.logit.step$predict, test$DEFAULT)
perf2 <- performance(perf1,"tpr","fpr") 
plot(perf2)

#Set threshold based on train predictions
temp<-data.frame(predict=predict(object=model.logit.step, newdata=train.log, type='response')) #response gives probs for class: glm
cutoffs <- seq(0.001, .4, .005)
accuracy <- NULL
recall <- NULL
precision <- NULL
f1_score  <- NULL
for (i in seq(along = cutoffs)) {
  prediction <- ifelse(temp$predict>=cutoffs[i], 1,0)
  accuracy   <- c(accuracy, 
              length(which(train$DEFAULT==prediction))/length(prediction)*100)
  recall     <- c(recall,
              length(which(prediction==1  & train$DEFAULT==1))/length(which(train$DEFAULT==1))*100) #TP / Act P
  precision  <- c(precision,
                 length(which(prediction==1 & train$DEFAULT==1))/length(which(prediction==1))*100) #TP / Pred P
  f1_score   <- c(f1_score, 2*(   (precision[i]*recall[i])/(precision[i]+recall[i])  ))
}

plot(cutoffs, f1_score, pch = 19, type='b', col='steelblue',
      xlab='Cutoff Level', ylab='Train F1 Score')
plot(cutoffs, accuracy, pch = 19, type='b', col='steelblue',
     xlab='Cutoff Level', ylab='Train F1 Score')
plot(cutoffs, precision, pch = 19, type='b', col='steelblue',
     xlab='Cutoff Level', ylab='Train F1 Score')
plot(cutoffs, recall, pch = 10, type='b', col='steelblue',
     xlab='Cutoff Level', ylab='Metric')
lines(cutoffs, accuracy, pch = 10, type='b', col='orange')
legend('bottom',legend=c('Recall','Accuracy'),
       text.col=c('steelblue','orange'), col=c('steelblue','orange'),bty='n',pch=c(1,1),
       cex=.8)

train.log.metrics<-data.frame(cutoffs,accuracy,recall,precision, f1_score)
temp<-train.log.metrics[57:61,]
temp

file.name  <-'accuracy and recall.html'
stargazer(temp, 
          type=c('html'), out=paste(out.path, file.name,sep=''),
          title=c('Stepwise Logistic Metrics on Train'),
          align=TRUE, digits = 3,digits.extra=2, initial.zero=TRUE, summary = FALSE)


roc_obj <- roc(train$DEFAULT, temp$predict)

'''
optimal.log.threshold<-cutoffs[which(max(f1_score) == f1_score)[[1]]]
optimal.log.threshold #top threshold
f1_score[which(max(f1_score) == f1_score)] #top f1 score
'''

optimal.log.threshold<-.29
preds.logit.step2<-preds.logit.step 
preds.logit.step2$predict<-ifelse(preds.logit.step2$predict>=optimal.log.threshold,1,0)

preds.logit.step2['actual']<-as.numeric(as.character(test$DEFAULT))
preds.logit.step2['0']<-NULL
preds.logit.step2['model']<-"Logit"
names(preds.logit.step2)<-c('predict','actual', 'model')


# Logistic Regression Evaluation Metrics
auc(preds.logit.step$actual,preds.logit.step$predict) #generates AUC

#Make ROC plot.  All Method Results are the same.
temp<-rbind(preds.logit.back,
            preds.rf, 
            preds.gbm
            preds.svm)
# preds.knn,
# preds.nn,
# preds.nb)

ggplot(temp, aes(d = actual, m = predict, color=model)) + geom_roc(n.cuts = 0)

#Confusion Matrix - Test
confusionMatrix(data = preds.rf$predict, reference = factor(preds.rf$actual), mode = "prec_recall")
confusionMatrix(data = preds.gbm$predict, reference = factor(preds.gbm$actual), mode = "prec_recall")
confusionMatrix(data = preds.knn$predict, reference = factor(preds.knn$actual), mode = "prec_recall")
confusionMatrix(data = preds.logit.step2$predict, reference = factor(preds.logit.step2$actual), mode = "prec_recall")


#validation false positive rates
384/(5382+384) #rf 
311/(5455+311)#gbm
331/(5435+331)#knn
1289/(1289+4477)#logistic


#model visualizations
plot(model.rf) #cv scores for number of predictors at each node
plot(model.rf2) # train error as trees increases
data.frame(model.rf2$err.rate)
data.frame(model.rf2$err.rate)
#mean OOB
sum(data.frame(model.rf2$err.rate)['OOB'])/nrow(data.frame(model.rf2$err.rate)['OOB'])
sum(data.frame(model.rf2$err.rate)['X0'])/nrow(data.frame(model.rf2$err.rate)['X0'])
sum(data.frame(model.rf2$err.rate)['X1'])/nrow(data.frame(model.rf2$err.rate)['X1'])
data.frame(model.rf2$err.rate)
model.knn$metric
plot(model.gbm)
plot(model.knn)
head(summary(model.gbm2,order=TRUE),5) #feature importances


temp<-data.frame(importance(model.rf2,type=2)) #type=1 MeanDecreaseAccuracy #type=2 MeanDecreaseGini
temp1<-row.names(temp) #assign row names
temp <- data.frame(MeanDecreaseGini=temp[order(-temp$MeanDecreaseGini),],
                              row.names=temp1) #order ascending
varImpPlot(model.rf2,type=2)
varImpPlot


#Train Metrics - Producing ROC Plot for Train Split (to Compare with Train)
preds.rf.train<-data.frame(predict(object=model.rf2, newdata=X.train))
preds.rf.train['actual']<-as.numeric(as.character(train$DEFAULT))
preds.rf.train['0']<-NULL
preds.rf.train['model']<-"rf"
names(preds.rf.train)<-c('predict','actual', 'model')

preds.gbm.train<-data.frame(predict(object=model.gbm2, newdata=X.train))
preds.gbm.train['actual']<-as.numeric(as.character(train$DEFAULT))
preds.gbm.train['0']<-NULL
preds.gbm.train['model']<-"gbm"
names(preds.gbm.train)<-c('predict','actual', 'model')

preds.knn.train<-data.frame(predict(object=model.knn2, newdata=X.train))
preds.knn.train['actual']<-as.numeric(as.character(train$DEFAULT))
preds.knn.train['0']<-NULL
preds.knn.train['model']<-"knn"
names(preds.knn.train)<-c('predict','actual', 'model')

#Train for logistic (MORE INVOLVED)
X.train.log<-train.log[1:15]
preds.logit.step.train<-data.frame(predict(object=model.logit.step, newdata=X.train.log,type='response'))
preds.logit.step.train['actual']<-as.numeric(as.character(train.log$DEFAULT))
preds.logit.step.train['0']<-NULL
preds.logit.step.train['model']<-"Logit"
names(preds.logit.step.train)<-c('predict','actual', 'model')
preds.logit.step.train

perf1 <- prediction(preds.logit.step.train$predict, train$DEFAULT)
perf2 <- performance(perf1,"tpr","fpr") 
plot(perf2)

'''
cutoffs <- seq(0.1, .9, .1)
accuracy.train <- NULL
for (i in seq(along = cutoffs)) {
  prediction <- ifelse(
    preds.logit.step.train$predict>=cutoffs[i], 1,0)
  accuracy.train <- c(accuracy.train, 
                length(which(train$DEFAULT==prediction))/length(prediction)*100)}

plot(cutoffs, accuracy.train, pch = 19, type='b', col='steelblue',
     xlab='Cutoff Level', ylab='Train Accuracy')
optimal.log.threshold.train<-cutoffs[which(max(accuracy.train) == accuracy.train)[[1]]]
preds.logit.step2.train<-preds.logit.step.train
'''
preds.logit.step2.train$predict<-ifelse(preds.logit.step.train$predict>=.29,1,0)
preds.logit.step2.train


confusionMatrix(data = preds.rf.train$predict, reference = factor(preds.rf.train$actual), mode = "prec_recall")
confusionMatrix(data = preds.gbm.train$predict, reference = factor(preds.gbm.train$actual), mode = "prec_recall")
confusionMatrix(data = preds.knn.train$predict, reference = factor(preds.knn.train$actual), mode = "prec_recall")
confusionMatrix(data = preds.logit.step2.train$predict, reference = factor(preds.logit.step2.train$actual), mode = "prec_recall")




#train false positives
566/(11191+566)#gbm
1/(1+11756) #rf
513/(513+11244)#knn
2547/(9210+2547)#logistic





# Final Performance Estimate - GBM -------------------------------------

control <- trainControl(method="cv", number=3)
set.seed(1) #use on line immediately prior to code chunk with variable results to make results constant
model.gbm.f <- train(DEFAULT~., data=traintest, method="gbm",  
                   trControl=control, tuneLength=5,na.action=na.pass) 
set.seed(1)
model.gbm.f2 <- train(DEFAULT~., data=traintest,method='gbm',
                    trControl=trainControl(method="none"),
                    tuneGrid=
                      data.frame(shrinkage=model.gbm.f$bestTune$shrinkage,
                                 n.minobsinnode=model.gbm.f$bestTune$n.minobsinnode,
                                 n.trees=model.gbm.f$bestTune$n.trees,
                                 interaction.depth=model.gbm.f$bestTune$interaction.depth))

preds.gbm.f<-data.frame(predict(object=model.gbm.f2, newdata=X.validate,n.trees=model.gbm.f2$n.trees))
preds.gbm.f['actual']<-as.numeric(as.character(validate$DEFAULT))
preds.gbm.f['0']<-NULL
preds.gbm.f['model']<-"GBM"
names(preds.gbm.f)<-c('predict','actual', 'model')

confusionMatrix(data = preds.gbm.f$predict, reference = factor(preds.gbm.f$actual), mode = "prec_recall")
#false positive rate
283/(283+5558)

preds.gbm.f.train<-predict(object=model.gbm.f2, newdata=X.traintest, type='prob')
preds.gbm.f.train['actual']<-as.numeric(as.character(y.traintest))
preds.gbm.f.train['0']<-NULL
preds.gbm.f.train['model']<-"gbm"
names(preds.gbm.f.train)<-c('predict','actual', 'model')

confusionMatrix(data = preds.gbm.f.train$predict, reference = factor(preds.gbm.f.train$actual), mode = "prec_recall")
#false positive rate
preds.gbm.f.train

as.numeric((as.character(y.traintest)))




