rm(list=ls())
library(ggplot2)
library("rgdal")
setwd(".")

RI<-readOGR("muni97d/muni97d.shp")
par(mar=c(0,0,0,0))
plot(RI)

#get coronavirus data
cov<-read.csv("datasets/covid_2020-04-10.csv",header=T, stringsAsFactors = F)
#all "<5" temp set to 3
cov$Number.cont<-cov$Number
cov$Number.cont[cov$Number=="<5"]<-"3"
cov$Number.cont<-as.numeric(as.character(cov$Number.cont))

library("sendplot")

#create colors for heatmap
cov$colorBuckets <- as.numeric(cut(cov$Number.cont,c(-1,2,5,10,25,50,75,100,150,Inf)))
n<-length(c(-1,2,5,10,25,50,75,100,150,Inf))
colors = heat.colors(n, alpha = 1)
#switch around
colors<-colors[seq(n,1)]
cov$col.names<-colors[cov$colorBuckets]

#add to map data
library("stringr")
cov$town<-str_to_upper(cov$Town)
sum(cov$town%in%RI$NAME)
#all here
idmatch<-match(as.character(RI$NAME),cov$town)
RI$col<-cov$col.names[idmatch]

plot(RI,col=RI$col)
#legend

leg.text<-c("0","1-5","5-9","10-25","25-50","50-75","75-100","100-150",">150")
legend("topright",leg.text,fill=colors,cex=.9,bty="n")

#add the interactive elements
#get the midpoints of each location (centroids)
#library("sp","spdep")
library("rgdal")
library("geoR","lattice")
library("classInt","RColorBrewer")

#we want text to come up everywhere in a town
library(GISTools)
cents <- coordinates(RI)
cents <- SpatialPointsDataFrame(coords=cents, data=RI@data, 
                                proj4string=CRS("+proj=longlat +ellps=clrk66"))
points(cents, col = "Blue")
#some weird values- take a look
#order by area
cents<-cents[order(cents$NAME,cents$AREA,decreasing=T),]

centsUnique<-cents[!duplicated(cents$NAME),]
#add in the smaller portsmouth area
port<-cents[cents$NAME=="PORTSMOUTH",][2,]
centsUnique<-rbind(centsUnique,port)
#same for narragansett
narr<-cents[cents$NAME=="NARRAGANSETT",][2,]
centsUnique<-rbind(centsUnique,narr)
#warwick
eg<-cents[cents$NAME=="WARWICK",][2,]
centsUnique<-rbind(centsUnique,eg)

plot(RI)
points(centsUnique,col="blue")
#perfect!
#add numbers in
idmatch<-match(centsUnique$NAME,cov$town)
centsUnique$number<-cov$Number.cont[idmatch]
centsUnique$num.cat<-cov$Number[idmatch]

idmatch<-match(as.character(RI$NAME),cov$town)
RI$col<-cov$col.names[idmatch]

plot(RI,col=RI$col)


par(mar=c(2,0,0,0))
direct="./"

plot.call<-c("plot(RI,col=RI$col,main='RI COVID-19 Cases by Town',cex.main=2,xlab='April 10, 2020',cex.lab=3)")
plot.extras<-c("legend('topright',leg.text,fill=colors,cex=2,bty='n')")

#get annotations
x.pos<-coordinates(centsUnique)[,1]
y.pos<-coordinates(centsUnique)[,2]

xy.labels = data.frame(Town=centsUnique$NAME,Number=centsUnique$num.cat)

#spot.radius should vary by area
centsUnique$spots.radius<-10
centsUnique$spots.radius[centsUnique$AREA>=2.5e8&centsUnique$AREA<=6.8e8]<-15
centsUnique$spots.radius[centsUnique$AREA>6.8e8]<-20

        
xy.send(plot.call=plot.call,
        y.pos=y.pos,x.pos=x.pos,
        xy.labels = xy.labels,
        plot.extras=plot.extras,
        image.size="800x600",
        fname.root="plot",
        dir=direct,
        spot.radius=centsUnique$spots.radius,
        font.size=25)
