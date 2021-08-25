library("rnaturalearth")
library("rnaturalearthdata")

#read in shapefile, automatically reads in the first layer
GDB <- st_read("Data/Alaska_Marine_Management_Areas.gdb")
#filter to regions, if you don't do this df3 will end up with >1000 points
ESR <- GDB %>% filter(Area_Type=="Ecosystem Subarea")
LME <- GDB %>% filter(Area_Type=="Ecosystem Area")
NMFS <- GDB %>% filter(Area_Type=="NMFS Reporting Area")
ADFG <- GDB %>% filter(Area_Type=="ADFG Stat Area")
BSIERP <- GDB %>% filter(Area_Type=="BSIERP Region")

#Make maps
#bring in world data
US <- ne_countries(scale = "medium", returnclass = "sf", country="united states of america")
#plot ESR
png("www/ESR.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  geom_sf(data=ESR, aes(), fill=NA)+
  geom_sf_text(data=ESR, aes(label=Ecosystem_Subarea))+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw() +
  theme(axis.title =element_blank())
dev.off()

#plot LME
png("www/LME.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  geom_sf(data=LME, aes(), fill=NA)+
  geom_sf_text(data=LME, aes(label=Ecosystem_Area))+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()+
  theme(axis.title =element_blank())
dev.off()

#plot NMFS
png("www/NMFS.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+ 
  geom_sf(data=NMFS, aes(), fill=NA)+
 geom_sf_text(data=NMFS, aes(label=NMFS_REP_AREA))+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()+
  theme(axis.title =element_blank())
dev.off()

#plot ADFG
png("www/ADFG.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  geom_sf(data=ADFG, aes(), fill=NA)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()+
  theme(axis.title =element_blank())
dev.off()

#plot BSIERP
png("www/BSIERP.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  geom_sf(data=BSIERP, aes(), fill=NA)+
  geom_sf_text(data=BSIERP, aes(label=BSIERP_ID))+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()+
  theme(axis.title =element_blank())
dev.off()

#Plot crab areas
CRAB_SM<-st_read("Data/St_Matthew_District.shp")
CRAB_BB<-st_read("Data/BristolBay.shp")

png("www/CRAB.png", width=650, height=540, units="px")
ggplot()+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  geom_sf(data=CRAB_BB, aes(), fill=NA)+
  geom_sf(data=CRAB_SM, aes(), fill=NA)+
  annotate("text",x=4e6, y=8.5e6, label="St. Matthew")+
  annotate("text",x=5.1e6, y=7.7e6, label="Bristol Bay")+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()+
  theme(axis.title =element_blank())
dev.off()
