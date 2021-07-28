library(shiny)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(sp) # For maps
library(sf) #for maps
library(rgdal) # For maps


ui <- fluidPage(

  tags$blockquote("Point-in-polygonizer tool"),
  tags$blockquote("Point in polygon operations are increasingly used in fisheries research and management. To address this need, we present a tool that adds commonly used fisheries area attributes to submitted points"),
  tags$blockquote("Simply submit a csv with coordinates using the 'input' button, and this tool will return the file with an additional column indicating in which polygon each point falls"),
  tags$blockquote("Coodinate fields must be titled 'latitude' and 'longitude', the tool is case sensitive, and the coordinate format must be in decimal degrees"),
  downloadButton("updated_data","Download polygonized data")
  
  )

server <- function(input, output) {
  
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
  
  #Download buttons
  #csv
  output$updated_data<-downloadHandler(
    filename = function() {
      paste("updated_data.csv")
    },
    content = function(file) {
      write.csv(updated_data, file)
    })
}

shinyApp(ui = ui, server = server)

#testing
#Download buttons
#csv
output$updated_data<-downloadHandler(
  filename = function() {
    paste("updated_data.csv")
  },
  content = function(file) {
    write.csv(updated_data, file)
  })



#read in fgdb
area<- readOGR(dsn="Data/Alaska_Marine_Management_Areas.gdb",
               layer="Alaska_Marine_Areas_dd",
               verbose=FALSE)
#convert to data frame
test.df <- merge(fortify(area), as.data.frame(area), by.x="id", by.y=0) %>%
  mutate(long2=ifelse(long>0,long-360, long),
         BSIERP_ID=ifelse(BSIERP_ID==0,NA,BSIERP_ID)) %>% 
  rename(`NMFS Area`=NMFS_REP_AREA,
         `ESR Region`=Ecosystem_Subarea,
         `ADFG Stat Area`=STAT_AREA,
         `BSIERP Region`=BSIERP_ID)


#let's just make sure the operation works
df <- read.csv("test.csv", header = TRUE)%>%
  mutate(LAT=latitude, LON=longitude)

#  define as shapefile and set a crs of WGS84
points_sf = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
#read in shapefile
adfg <- st_read('Data/adfg_stat_areas_simple.shp')

#  Transform the points to match the bsierp regions projection
points_sf <- points_sf %>% st_transform(st_crs(adfg)$proj4string)
#  Match the poiints to areas
df_in <- st_join(points_sf, adfg, join = st_within)
#convert back to data frame
df_in <-df_in%>% 
  data.frame%>%
  dplyr::select(!c(OBJECTID,GEOMETRY_S,GEOMETRY_E,FISHERY_GR,GIS_SERIES,GIS_SERI_1,
                   REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, REGISTRA_3, 
                   DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                   SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                   INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_2,
                   IFQ_SABLEF,COAR_AREA_,CREATOR,CREATE_DAT,EDITOR,   
                   EDIT_DATE,COMMENTS,STAT_AREA_,Shape__Are,Shape__Len,geometry))


ggplot() + 
  geom_sf(data = df_in) +
  geom_sf(data = adfg,col="white",fill=NA)

