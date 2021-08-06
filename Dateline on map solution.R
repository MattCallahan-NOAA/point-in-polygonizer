require(raster)
require(sp)
require(sf)
require(rgdal)
require(tidyverse)
require(nngeo)
#merge ploygons

xmin <- 165
xmax <- 230
ymin <- 50
ymax <- 68

#  Load the non-Crab areas
area<- readOGR(dsn="Data/Alaska_Marine_Management_Areas.gdb",
               layer="Alaska_Marine_Areas_dd",
               verbose=FALSE)
test.df <- merge(fortify(area), as.data.frame(area), by.x="id", by.y=0) %>%
  mutate(long2=ifelse(long>0,long-360, long),
         BSIERP_ID=ifelse(BSIERP_ID==0,NA,BSIERP_ID)) %>% 
  rename(`NMFS Area`=NMFS_REP_AREA,
         `ESR Region`=Ecosystem_Subarea,
         `ADFG Stat Area`=STAT_AREA,
         `BSIERP Region`=BSIERP_ID)
#play with data frame... little succsess
ESR2<-test.df %>% filter(Area_Type=="Ecosystem Subarea")

ggplot() + 
  geom_polygon(data=ESR2,aes(long2,lat,group=factor(group)),
               fill=NA,
               color="black") + 
  coord_map("albers",lat0=54,lat1=62,xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_void()+
  theme(legend.position = "none")

CAI2<-ESR2 %>% filter(`ESR Region`=="Central Aleutians") 


  ggplot(CAI2) + 
  geom_polygon(aes(long2,lat,group=factor(group), fill=factor(group)),
               #fill=NA,
               color="black") + 
  coord_map("albers",lat0=54,lat1=58,xlim=c(175,190),ylim=c(48,56)) + 
  theme_void()
  

  #FAILED
 # CAI2<-CAI2%>%mutate(group=fct_recode(group, "1783.2"="1783.1"))
  
  ggplot() + 
    geom_polygon(data=tidy(area) %>% # Load basemap
                   filter(Area_Type=="Ecosystem Subarea")%>%
                   mutate(long2=ifelse(long<0,long+360,long))
                 ,aes(x=long2,y=lat,group=factor(group)),fill="grey70")

  
 
  ggplot(CAI2) + 
    geom_polygon(aes(long2,lat,group=group,color=group),
                 #fill=NA,
                 ) + 
    coord_map("albers",lat0=54,lat1=58,xlim=c(175,190),ylim=c(48,56)) + 
    theme_void()
  
  #try just working on it as a spatial opject
  plot(area[1783,])
  CAI3<-area[1783,]
  CAI3<-aggregate(CAI3)
  
  ggplot() + 
    geom_polygon(data=tidy(CAI3) %>% # Load basemap
                   #filter(Area_Type=="Ecosystem Subarea")%>%
                   mutate(long2=ifelse(long<0,long+360,long))
                 ,aes(x=long2,y=lat,group=factor(group)),fill="grey70", color="black")

#  Load and merge the two different crab shapefiles.
# crab <- readOGR(dsn="../../Other_People/Erin_Fedewa/Data",layer="BristolBay") %>% 
#   fortify() %>% 
#   mutate(long2=ifelse(long<0,long+360, long),
#          group="bb") %>% 
#   bind_rows(readOGR(dsn="../../Other_People/Erin_Fedewa/Data",layer="St_Matthew_District") %>% 
#               fortify() %>% 
#               mutate(long2=ifelse(long<0,long+360,long),
#                      group="stm"))

#  Merge the different polygon fields and shapefiles.
newdata <- test.df %>% 
  filter(!is.na(`NMFS Area`)) %>% 
  mutate(stratum="NMFS Areas") %>% 
  bind_rows(test.df %>% 
              filter(!is.na(`BSIERP Region`)) %>% 
              mutate(stratum="BSIERP Regions")) %>% 
  bind_rows(test.df %>% 
              filter(!is.na(`ESR Region`)) %>% 
              mutate(stratum="ESR Regions")) %>% 
  bind_rows(test.df %>% 
              filter(!is.na(`ADFG Stat Area`)) %>% 
              mutate(stratum="ADF&G Stat Areas")) %>% 
  bind_rows(readOGR(dsn="Data",layer="BristolBay",verbose=FALSE) %>%  # Read in and mergee the crab shapefiles
              fortify() %>% 
              mutate(long2=ifelse(long<0,long+360, long),
                     group="bb") %>% # To avoid duplicating group factors from the other shapefiles, create a distinct grouping level for crab. bb is Bristol Bay
              bind_rows(readOGR(dsn="Data",
                                layer="St_Matthew_District",
                                verbose=FALSE) %>%
                          fortify() %>% 
                          mutate(long2=ifelse(long<0,long+360,long),
                                 group="stm")) %>% # Similar to Bristol Bay, create St. Matthews grouping factor
              mutate(stratum="Crab Mgmt Areas"))

ggplot() + 
  geom_polygon(data=tidy(readOGR(dsn="Data",
                                 layer="AKbasemap",
                                 verbose=FALSE)) %>% # Load basemap
                 mutate(long2=ifelse(long<0,long+360,long))
               ,aes(x=long2,y=lat,group=factor(group)),fill="grey70") +
  geom_polygon(data=newdata,aes(long2,lat,group=factor(group)),
               fill=NA,
               color="black") + 
  facet_wrap(~stratum,ncol=2) +
  coord_map("albers",lat0=54,lat1=62,xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_void()



#####skip that shit and try to do it with sf
#read in shapefile, automatically reads in the first layer
GDB <- st_read("Data/Alaska_Marine_Management_Areas.gdb")
#filter to regions, if you don't do this df3 will end up with >1000 points
ESR <- GDB %>% filter(Area_Type=="Ecosystem Subarea")

CAI<-filter(ESR, Ecosystem_Subarea=="Central Aleutians")

ggplot(data=st_union(CAI))+geom_sf()+coord_sf(crs = "+init=epsg:3832", xlim = c(3e6, 4e6), ylim = c(6e6, 7.5e6), ndiscr=0)+
  theme_bw() 


#st_shift_longitude-fucking awesome for AK. 
ESR %>% st_shift_longitude() %>%
  ggplot()+geom_sf(aes(fill=Ecosystem_Subarea))

plot(st_combine(CAI)%>% st_shift_longitude())

CAI4<-st_remove_holes(CAI)

CAI4 %>% st_shift_longitude() %>%
  ggplot()+geom_sf(aes(fill=Ecosystem_Subarea))

CAI5<-matrix(c(st_coordinates(st_union(st_shift_longitude(CAI)))[,1],st_coordinates(st_union(st_shift_longitude(CAI)))[,2]), ncol=2)

plot(CAI5)

CAI6<-chull(CAI5)

CAI6<-c(CAI6, CAI6[1])

plot(CAI4 %>% st_shift_longitude())
lines(CAI5[CAI6,], col="red")

#test state
ADFG <- GDB %>% filter(Area_Type=="NMFS Reporting Area")

ADFG %>% st_shift_longitude() %>%
  ggplot()+geom_sf()+
  geom_sf_label(aes(label=NMFS_REP_AREA))

#This another way to join separate polygons, but doesn't seem to work with this.
library(rgeos)
library(UScensus2000tract)

# load data
data("oregon.tract")

# plot map
plot(oregon.tract)
# Dissolve all polygons
d <- gUnaryUnion(CAI3, id=CAI3)

ggplot() + 
  geom_polygon(data=tidy(d) %>% # Load basemap
                 #filter(Area_Type=="Ecosystem Subarea")%>%
                 mutate(long2=ifelse(long<0,long+360,long))
               ,aes(x=long2,y=lat,group=factor(group)),fill="grey70", color="black")
