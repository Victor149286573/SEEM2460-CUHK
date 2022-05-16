install.packages("tidyverse")
install.packages("dplyr")
install.packages("stringr")
install.packages("plotrix")


library(stringr)
library(tidyverse)
library(magrittr)
library(plotrix)
library(dplyr)


#Read data
library(readxl)
data3<- read_excel("D:/3 School/SEEM2460/data/code/data 3.xlsx")
country<-unlist(data5[duplicated(data5[1])==FALSE,1])
for(i in 1:nrow(data3))
{
  if(data3[i+2,3]!=data3[i,3]) break
  else {
    data3[i,13]=data3[i+2,11]-data3[i,11]
    data3[i,14]=data3[i+2,12]-data3[i,12]
    if(data3[i,13]>0) data3[i,15]=1
    if(data3[i,13]==0) data3[i,15]=0
    if(data3[i,13]<0) data3[i,15]=-1
    if(data3[i,14]>0) data3[i,16]=1
    if(data3[i,14]==0) data3[i,16]=0
    if(data3[i,14]<0) data3[i,16]=-1
    }}
View(data3)

#data selection
data4<-data3[,c(3,5,7,15,16)]
data4<-data4[-which((data4$`case signal`==0)==TRUE),]
data4<-data4[-which((data4$`death signal`==0)==TRUE),]
view(data4)
data5<-data4[complete.cases(data4),]

#########Sometimes Freq have three lines, but sometimes only two lines
##Define positive as 1 and negative as -1
##case
#Traing data
set.seed(2460)#set seed 2460
index <- sample(2,nrow(data5),replace = TRUE,prob=c(0.7,0.3))
traindata <- data5[index==1,]
testdata <-  data5[index==2,]
traindata_labels<-data5[index==1,4]
testdata_labels<-data5[index==2,4]

library(ggplot2)
ggplot(traindata, aes(x=first_dose_per, y=second_dose_per, colour=`case signal`)) + geom_point()+ggtitle("The vectors in the training data for cases and vaccine")#divided by colour
ggplot(testdata, aes(x=first_dose_per, y=second_dose_per, colour=`case signal`)) + geom_point()+ggtitle("The vectors in the testing data for cases and vaccine")#divided by colour


#data analysis
library("class")
result<-c()
for (i in 1:round(sqrt(dim(data5)[1]))){
       model <- knn(train = traindata[,c(2,3)], test = testdata[,c(2,3)],  cl = unlist(traindata_labels), k = i)
        #change label into list instead of data frame
       Freq <- table(unlist(testdata_labels), model)
       c<-c()
       if(nrow(Freq)==ncol(Freq))
       c<-c(i,sum(diag(Freq))/sum(Freq),Freq[2,2]/(sum(Freq[,2])),Freq[2,2]/(sum(Freq[2,])),2*Freq[2,2]/(sum(Freq[,2])+sum(Freq[2,]))) 
       result<-rbind(result,c)
       print(c)
       #accuracy,precision,recall,f-measure where we want to find whether vacination has a positive effect on the epdemic
}
indexx<-which.max(result[,2])
indexx
result<-as.data.frame(result)
names(result)<-c("k","accuracy","precision","recall","F-measure")
ggplot(data = result, mapping = aes(x = k, y = accuracy, group = 1)) + geom_line() + xlab('k') + ylab('accuracy')+ggtitle('The accuracy of KNN model for cases and vaccine in different k')


##death
#Traing data
set.seed(2460)
result<-c()
index <- sample(2,nrow(data5),replace = TRUE,prob=c(0.7,0.3))
traindata <- data5[index==1,]
testdata <-  data5[index==2,]
traindata_labels<-data5[index==1,5]
testdata_labels<-data5[index==2,5]

ggplot(traindata, aes(x=first_dose_per, y=second_dose_per, colour=`death signal`)) + geom_point()+ggtitle("The vectors in the training data for deaths and vaccine")#divided by colour
ggplot(testdata, aes(x=first_dose_per, y=second_dose_per, colour=`death signal`)) + geom_point()+ggtitle("The vectors in the testing data for deaths and vaccine")#divided by colour


library("class")
for (i in 1:round(sqrt(dim(data5)[1]))){
  model <- knn(train = traindata[,c(2,3)], test = testdata[,c(2,3)],  cl = unlist(traindata_labels), k = i)
  Freq <- table(unlist(testdata_labels), model)
  c<-c()
  #change label into list instead of data frame
  c<-c(i,sum(diag(Freq))/sum(Freq),Freq[2,2]/(sum(Freq[,2])),Freq[2,2]/(sum(Freq[2,])),2*Freq[2,2]/(sum(Freq[,2])+sum(Freq[2,]))) 
  result<-rbind(result,c)
  print(c)
  #accuracy,precision,recall,f-measure
}
indexx<-which.max(result[,2])
indexx
result<-as.data.frame(result)
names(result)<-c("k","accuracy","precision","recall","F-measure")
ggplot(data = result, mapping = aes(x = k, y = accuracy, group = 1)) + geom_line() + xlab('k') + ylab('accuracy')+ggtitle('The accuracy of KNN model for deaths and vaccine in different k')

