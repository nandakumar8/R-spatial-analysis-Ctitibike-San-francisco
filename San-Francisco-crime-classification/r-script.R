
library(ggplot2)
library(dplyr)
library(ggmap)
library(plyr)
library(lattice)
###read data

trainData<-read.csv("C:\\Users\\Nandakumar\\Documents\\GitHub\\San-Francisco-Crime-Classification\\train.csv\\train.csv",header=T,stringsAsFactors=F)

testData<-read.csv("C:\\Users\\Nandakumar\\Documents\\GitHub\\San-Francisco-Crime-Classification\\test.csv\\test.csv",header=T,stringsAsFactors=F)

sf_map<-get_map("san francisco",zoom=12,maptype="toner-hybrid",source="stamen")
sf_map<-ggmap(sf_map)

head(trainData)

###use aggregate to find the total count of each category of crime
df<-aggregate(Dates~Category,trainData,length)
###find the top twenty crimes in the city
df<-df[order(-df$Dates),]
###removing other offenses
df<-df[-2,]
###take the top twenty offenses
df<-df[1:10,]
head(df)
tmp<-df$Category
###create map data with 
map_data<-trainData[trainData$Category %in% tmp,]
head(map_data)
sf<-sf_map+geom_point(aes(x=X,y=Y,color=factor(Category)),alpha=0.05,data=map_data)
sf+guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),title="Type of Crime"))

###bar plot of the crimes
bar<-ggplot(df,aes(x=df$Category,y=df$Dates,fill=df$Category))
bar+geom_bar(stat="identity")+ggtitle(label = "total count by crime")+
theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0),axis.text.x=element_blank())+
xlab(label ="Category")+ylab(label = "Count")
