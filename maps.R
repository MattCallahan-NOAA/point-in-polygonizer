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
ggplot()+
  geom_sf(data=ESR, aes(), fill=NA)+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw() 

#plot LME
ggplot()+
  geom_sf(data=LME, aes(), fill=NA)+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()

#plot NMFS
ggplot()+
  geom_sf(data=NMFS, aes(), fill=NA)+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()

#plot ADFG
ggplot()+
  geom_sf(data=ADFG, aes(), fill=NA)+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()

#plot BSIERP
ggplot()+
  geom_sf(data=BSIERP, aes(), fill=NA)+
  geom_sf(data=US, color="gray", inherit.aes = FALSE)+
  coord_sf(crs = "+init=epsg:3832", xlim = c(2e6, 8.7e6), ylim = c(6e6, 11.3e6), ndiscr=0)+
  theme_bw()
