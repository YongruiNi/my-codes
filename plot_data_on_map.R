library(ggmap)
library(ggplot2)
sc <- readRDS("C:/Users/Administrator.SC-201301200557/Desktop/3/cl_apartments.rds")
#set davis subset from data
davis<- sc[ which(sc$latitude>=38.516881 & sc$latitude<=38.5888 & sc$longitude<=-121.674847 & sc$longitude>=-121.794739), ]

# set map location of Davis
bbox = c(-121.80,38.51,-121.67,38.58)
m = get_stamenmap(bbox, zoom = 12)
ggmap(m)

# plot the parking kind of the apts
pdavis<-ggmap(m)+geom_point(data=davis,aes(x=longitude,y=latitude,color=parking),alpha=1)
pdavis

#plot the bedroom from map
bdavis<-ggmap(m)+geom_point(data=davis,aes(x=longitude,y=latitude,color=bedrooms),alpha=1)
bdavis

#plot the price from map
prdavis<-ggmap(m)+geom_point(data=davis,aes(x=longitude,y=latitude,color=price),alpha=1)
prdavis


#set sf bay are subset form data
sfb<- sc[ which(sc$latitude>=37.30 & sc$latitude<=37.96 & sc$longitude<=-121.70 & sc$longitude>=-122.55 & sc$price<=5000 & sc$price>=500&sc$parking!='NA'& sc$bedrooms!='NA'), ]

# set map location of sf bay area
bboxs = c(-122.55,37.50,-119.9,37.86)
s = get_stamenmap(bboxs, zoom = 12)
ggmap(s)

# plot the parking kind of the apts
psfb<-ggmap(s)+geom_point(data=sfb,aes(x=longitude,y=latitude,color=parking),alpha=1)
psfb

#plot the bedroom from map
bsfb<-ggmap(s)+geom_point(data=sfb,aes(x=longitude,y=latitude,color=bedrooms),alpha=1,size=1)
bsfb

#plot the price from map
prsfb<-ggmap(s)+geom_point(data=sfb,aes(x=longitude,y=latitude,color=price),alpha=1)
prsfb

#get the citys we want
southb<-southbay[ which(southbay$GEO.id2=='0667000'
                        |southbay$GEO.id2=='0673262'
                        |southbay$GEO.id2=='0668252'
                        |southbay$GEO.id2=='0669084'
                        |southbay$GEO.id2=='0600562'
                        |southbay$GEO.id2=='0604470'
                        |southbay$GEO.id2=='0622454'
                        |southbay$GEO.id2=='0668263'
                        |southbay$GEO.id2=='0683215'), ]
# calculat the total percentage of people over 40
old_people=as.numeric(southb$HD02_S025)++as.numeric(southb$HD02_S010)+as.numeric(southb$HD02_S011)+as.numeric(southb$HD02_S012)+as.numeric(southb$HD02_S013)+as.numeric(southb$HD02_S014)

#make a table with location and percentage
oldp <- data.frame(
  name = I(c("Alameda", "Bayview", "El Sobrante",
             "San Francisco", "San Mateo", "San Miguel", "Santa Clara","South San Francisco","Contra Costa")),
  
  over40 = old_people)