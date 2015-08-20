

library(ggmap)
library(plyr)
###read csv file
nyc<-read.csv("C:\\Users\\Nandakumar\\Desktop\\201501-citibike-tripdata\\201501-citibike-tripdata.csv",header=T,stringsAsFactors=F)

###factor station levels to get count
nyc$start.station.id<-factor(nyc$start.station.id)

###plot nyc ggmap
nyc_map<-get_map("new york city",zoom=13,maptype ="toner-background",source="stamen")
nyc_map<-ggmap(nyc_map)

###prepare data
lat<-nyc$start.station.latitude
lon<-nyc$start.station.longitude
nyc_data<-nyc[,c("tripduration","start.station.id","start.station.name","start.station.latitude","start.station.longitude")]

head(nyc)
head(nyc_data)


###plot map
nyc_map+geom_point(aes(x=start.station.longitude,y=start.station.latitude,size=count(nyc_data$tripduration)),data=nyc_data)
###get count of trip duration or no of trips by factor of station id
tmp<-aggregate(tripduration~start.station.id,nyc_data,length)
tmp<-tmp[,1:2]
head(tmp)
###find unique latitude and longitude for each station id
tmploc<-aggregate(cbind(start.station.latitude,start.station.longitude)~start.station.id,nyc_data,unique)
nyc_tran<-data.frame(tmp,tmploc[,2:3])
names(nyc_tran)[2]<-"total_trip"

head(nyc_tran)
nyc_map+geom_point(aes(x=start.station.longitude,y=start.station.latitude,size=total_trip,color=total_trip),data=nyc_tran)+
ggtitle(label = "total trips at citibike stations")+
theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))

new<-aggregate(start.station.name~start.station.id,nyc,unique)
head(new)
nyc_tran$station.name<-new$start.station.name

###bar plot of the stations
bar<-ggplot(top_ten,aes(x=start.station.id,y=total_trips,fill=start.station.id))
bar+geom_bar(stat="identity")+ggtitle(label = "total trips count in top ten stations")+
theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))+
geom_text(aes(label=station.name),angle=90,hjust=1.5)


###geneder usage
head(nyc)
male<-nyc[nyc$gender==1,]
head(male)
###find count of trips by men
male<-aggregate(gender~start.station.id,length,data =male)
###sort in descending order
maleorder<-male[order(male$gender,decreasing =T),]
topmale<-male[1:10,]

female<-nyc[nyc$gender==2,]
head(female)
###find count of trips by women
female<-aggregate(gender~start.station.id,length,data =female)
###sort in descending order
femaleorder<-female[order(female$gender,decreasing =T),]
topfemale<-female[1:10,]

df<-data.frame(male,female)
df<-subset(df,select=-start.station.id.1)
colnames(df)[3]<-"female"
colnames(df)[2]<-"male"
###heatmap to show usage in sample 25 stations based on gender
row.names(df)<-df$start.station.id
df<-data.matrix(df)
heatmap(df[1:25,2:3],Rowv =NA,Colv=NA,col=cm.colors(256),scale="column",margins=c(10,6))

###ggmap to show usage by men and women
male_map<-get_map("new york city",zoom=13,maptype ="toner",source="stamen")
male_map<-ggmap(male_map)
maledf<-data.frame(male,tmploc[,2:3])
maledforder<-maledf[order(maledf$gender,decreasing=T),]
colnames(maledforder)[2]<-"count"
maledforder$gender<-1
femaledf<-data.frame(female,tmploc[,2:3])
femaledforder<-femaledf[order(femaledf$gender,decreasing=T),]
colnames(femaledforder)[2]<-"count"
femaledforder$gender<-2

###create data frame for ggmap
genderdf<-rbind(maledforder,femaledforder)  
male_map+geom_point(aes(x=start.station.longitude,y=start.station.latitude,size=count,color=factor(gender)),data=genderdf)+scale_color_hue("gender",l=70,c=150)+
scale_size(range=c(2,10))+ggtitle(label = "total usage Male(1) vs Female(2)")+
theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))

###bar plot to show top ten stations used by men
top_ten_male<-ggplot(maledforder[1:10,],aes(x=start.station.id,y=count,fill=start.station.id))
top_ten_male+geom_bar(stat="identity")+ggtitle(label = "top ten stations used by men")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))

###bar plot to show top ten station used by women
top_ten_female<-ggplot(femaledforder[1:10,],aes(x=start.station.id,y=count,fill=start.station.id))
top_ten_female+geom_bar(stat="identity")+ggtitle(label = "top ten stations used by women")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))

percdf<-cbind(male,female)
colnames(percdf)[2]<-"male"
colnames(percdf)[4]<-"female"
percdf<-subset(percdf,select=-3)
total_trips<-sum(nyc_tran$total_trip)
male_total_trips<-sum(percdf$male)
female_total_trips<-sum(percdf$female)

pertmp<-data.frame(sex,count)
sex<-c("male","female")
count<-c(male_total_trips,female_total_trips)
percpie<-ggplot(pertmp,aes(x=1,y=count,fill=sex))
percpie+geom_bar(width=1,stat="identity",color="black")+coord_polar(theta='y')+ggtitle(label = "male vs female total trips division ")+
theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0))+
guides(fill=guide_legend(override.aes=list(colour=NA)))+
theme(axis.ticks=element_blank(),  
axis.title=element_blank(), 
axis.text.y=element_blank())

percdf$total<-percdf$male+percdf$female
percdf$maleper<-(percdf$male/percdf$total)*100
percdf$femaleper<-(percdf$female/percdf$total)*100
colnames(percdf)


newdf<-subset(percdf,select=-male)
newdf<-subset(newdf,select=-female)
newdf<-subset(newdf,select=-total)
tmpplot<-ggplot(percdf[1:30,],aes(x=start.station.id,y=maleper,color=start.station.id))
tmpplot+geom_bar(stat="identity")


count<-rbind(percdf$male,percdf$female)
percentage<-rbind(percdf$maleper,percdf$femaleper)
start.station.id<-rbind(percdf$start.station.id,percdf$start.station.id)

