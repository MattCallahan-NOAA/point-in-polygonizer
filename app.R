library(shiny)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(sp)
library(sf)
library(rgdal)

# Define UI for data upload app ----
ui <- fluidPage(
  
  titlePanel("point-in-polygonizer"),
  
  tabsetPanel(type="tabs",
              
              tabPanel("Ecosystem Subarea",  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      #download new data
      downloadButton("ESR_DL","Download updated data"),
      tags$hr(),
      #text
      tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. All three spatial bounds were calculated from an ADFG stat area shapefile using the WGS 1984 coordinate system"),
      tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
      # Output: map
      tags$hr(),
      
      imageOutput(outputId = "ESR_Map")
      ),

    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("ESR_table"),
      

      
  ))), 
  
  tabPanel("Large Marine Ecosystems",
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Select a file ----
               fileInput("file2", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               downloadButton("LME_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. All three spatial bounds were calculated from an ADFG stat area shapefile using the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr(),
               tableOutput("LME_table")),
             
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               imageOutput(outputId = "LME_Map")
               
             
           ))) 
           
))

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$ESR_Map<-renderImage({
    
    filename <- normalizePath(file.path('Figures/ESR.png'))
    # Return a list containing the filename and alt text
    list(src = filename)}, deleteFile = FALSE)
    
    output$LME_Map<-renderImage({
      
      filename <- normalizePath(file.path('Figures/LME.png'))
      # Return a list containing the filename and alt text
      list(src = filename)}, deleteFile = FALSE)
 
    
     data1<-reactive({
       
    #read in points
    df <- read.csv(input$file1$datapath,
                   header = TRUE)%>%
      mutate(LAT=latitude, LON=longitude)
    
    #  define as shapefile and set a crs of WGS84
    df = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
   
     #read in shapefile
   # adfg <- st_read('Data/adfg_stat_areas_simple.shp')
    

    #read in shapefile, automatically reads in the first layer
    GDB <- st_read("Data/Alaska_Marine_Management_Areas.gdb")
    #filter to regions, if you don't do this df3 will end up with >1000 points
    ESR <- GDB %>% filter(Area_Type=="Ecosystem Subarea")
    #LME <- GDB %>% filter(Area_Type=="Ecosystem Area")
    #NMFS <- GDB %>% filter(Area_Type=="NMFS Reporting Area")
    #ADFG <- GDB %>% filter(Area_Type=="ADFG Stat Area")
    #BSIERP <- GDB %>% filter(Area_Type=="BSIERP Region")
    
    #  Transform the points to match the adfg  projection
   df <- df %>% st_transform(st_crs(GDB)$proj4string)
   
    #  Match the points to areas
    df <- st_join(df, ESR, join = st_within)
    #convert back to data frame
    df <-df%>% 
      data.frame%>%
      dplyr::select(!c(ID,Area_Type,AreaID,Area_Name,SourceURL,SourceName,BSIERP_ID,BSIERP_ID,
                       BSIERP_Region_Name,Ecosystem_Area,NMFS_REP_AREA,
                       STAT_AREA,FISHERY_GR,REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, 
                       REGISTRA_3,DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                       SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                       INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_1,NMFS_REP_2,
                       IFQ_IPHC_A,IFQ_SABLEF,COAR_AREA_,Shape_Area,Shape_Length,geometry))

     })  
     
     data2<-reactive({
       

       #read in points
       df <- read.csv(input$file2$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       
       #  define as shapefile and set a crs of WGS84
       df = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
       
       #read in shapefile
       # adfg <- st_read('Data/adfg_stat_areas_simple.shp')
       
       
       #read in shapefile, automatically reads in the first layer
       GDB <- st_read("Data/Alaska_Marine_Management_Areas.gdb")
       #filter to regions, if you don't do this df3 will end up with >1000 points
       #ESR <- GDB %>% filter(Area_Type=="Ecosystem Subarea")
       LME <- GDB %>% filter(Area_Type=="Ecosystem Area")
       #NMFS <- GDB %>% filter(Area_Type=="NMFS Reporting Area")
       #ADFG <- GDB %>% filter(Area_Type=="ADFG Stat Area")
       #BSIERP <- GDB %>% filter(Area_Type=="BSIERP Region")
       
       #  Transform the points to match the adfg  projection
       df <- df %>% st_transform(st_crs(GDB)$proj4string)
       
       #  Match the poiints to areas
       df <- st_join(df, LME, join = st_within)
       #convert back to data frame
       df <-df%>% 
         data.frame%>%
         dplyr::select(!c(ID,Area_Type,AreaID,Area_Name,SourceURL,SourceName,BSIERP_ID,BSIERP_ID,
                          BSIERP_Region_Name,Ecosystem_Subarea,NMFS_REP_AREA,
                          STAT_AREA,FISHERY_GR,REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, 
                          REGISTRA_3,DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                          SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                          INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_1,NMFS_REP_2,
                          IFQ_IPHC_A,IFQ_SABLEF,COAR_AREA_,Shape_Area,Shape_Length,geometry))
       
     })  
    #ESR output
    output$ESR_DL<-downloadHandler(
    filename = function() {
      paste0(input$file1,"_polygonized.csv")
      },
    content = function(file) {
      write.csv(data1(), file)
      }
    )
  
    output$ESR_table <- renderTable({
      req(input$file1)
      return(head(data1()))
    })
    #LME output
    output$LME_DL<-downloadHandler(
      filename = function() {
        paste0(input$file2,"_polygonized.csv")
      },
      content = function(file) {
        write.csv(data2(), file)
      }
    )
    
    output$LME_table <- renderTable({
      req(input$file2)
      return(head(data2()))
    })
    

}
# Run the app ----
shinyApp(ui, server)

