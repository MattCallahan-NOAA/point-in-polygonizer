library(shiny)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(sf)
library(rgdal)

# Define UI for data upload app ----
ui <- fluidPage(
  
  titlePanel("point-in-polygonizer"),
  tags$blockquote("This shiny app adds marine region information to point location data. No data will be stored or saved in any way when they are uploaded. 5mb limit."),
  
  
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
      tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
      tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
      ),
    # Main panel for displaying outputs ----
    mainPanel(
      #map
      img(src='ESR.png', align="left"),
      tags$hr(),
      #output preview
      tableOutput("ESR_table")
  ))), 
  
  tabPanel("Large Marine Ecosystems",
           sidebarLayout(
             sidebarPanel(
               fileInput("file2", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               downloadButton("LME_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("THESE BOUNDARIES CURRENTLY INCLUDE INSIDE WATERS, BUT WILL BE UPDATED SOON"),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr()),
             mainPanel(
               img(src='LME.png', align="left"),
               tags$hr(),
               tableOutput("LME_table")
           ))),
  
  tabPanel("NMFS Areas",
           sidebarLayout(
             sidebarPanel(
               fileInput("file3", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               downloadButton("NMFS_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr()),
             mainPanel(
               img(src='NMFS.png', align="left"),
               tags$hr(),
               tableOutput("NMFS_table")
             ))),
  tabPanel("ADFG Statistical Areas",
           sidebarLayout(
             sidebarPanel(
               fileInput("file4", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               downloadButton("ADFG_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr()),
             mainPanel(
               img(src='ADFG.png', align="left"),
               tags$hr(),
               tableOutput("ADFG_table")
             ))),
  tabPanel("BSIERP Regions",
           sidebarLayout(
             sidebarPanel(
               fileInput("file5", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               downloadButton("BSIERP_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr()),
             mainPanel(
               img(src='BSIERP.png', align="left"),
               tags$hr(),
               tableOutput("BSIERP_table")
             ))),
  tabPanel("Crab Management Areas",
           sidebarLayout(
             sidebarPanel(
               fileInput("file6", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               downloadButton("CRAB_DL","Download updated data"),
               tags$hr(),
               tags$blockquote("This tool accepts latitude and longitude coordinates and adds selected area. Regions are calculated using a PSMFC Alaska Marine Management Areas file geodatabase with the WGS 1984 coordinate system"),
               tags$blockquote("Submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
               tags$hr()),
             mainPanel(
               img(src='CRAB.png', align="left"),
               tags$hr(),
               tableOutput("CRAB_table")
             )))
))

# Define server logic to read selected file ----
server <- function(input, output) {
    #ESR data
     data1<-reactive({
    #read in points
    df <- read.csv(input$file1$datapath,
                   header = TRUE)%>%
      mutate(LAT=latitude, LON=longitude)
    #  define as shapefile and set a crs of WGS84
    df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, agr = "constant") 
    
    #read in fgdb, automatically reads in the first layer (decimal degrees)
    GDB <- st_read("Data/Alaska_Marine_Management_Areas.gdb")
    #filter to regions, if you don't do this df3 will end up with >1000 points
    ESR <- GDB %>% filter(Area_Type=="Ecosystem Subarea")
    #LME <- GDB %>% filter(Area_Type=="Ecosystem Area")
    #NMFS <- GDB %>% filter(Area_Type=="NMFS Reporting Area")
    #ADFG <- GDB %>% filter(Area_Type=="ADFG Stat Area")
    #BSIERP <- GDB %>% filter(Area_Type=="BSIERP Region")

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
     
     #LME
     data2<-reactive({
       df <- read.csv(input$file2$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, agr = "constant") 
       LME <- st_read("Data/Alaska_Marine_Management_Areas.gdb")%>% 
         filter(Area_Type=="Ecosystem Area")
       df <- st_join(df, LME, join = st_within)
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
     
     #NMFS
     data3<-reactive({
       df <- read.csv(input$file3$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, agr = "constant") 
       NMFS <- st_read("Data/Alaska_Marine_Management_Areas.gdb")%>%
       filter(Area_Type=="NMFS Reporting Area")
       df <- st_join(df, NMFS, join = st_within)
       df <-df%>% 
         data.frame%>%
         dplyr::select(!c(ID,Area_Type,AreaID,Area_Name,SourceURL,SourceName,BSIERP_ID,BSIERP_ID,
                          BSIERP_Region_Name,Ecosystem_Subarea,Ecosystem_Area,
                          STAT_AREA,FISHERY_GR,REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, 
                          REGISTRA_3,DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                          SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                          INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_1,NMFS_REP_2,
                          IFQ_IPHC_A,IFQ_SABLEF,COAR_AREA_,Shape_Area,Shape_Length,geometry))
     })  
     
     #ADFG
     data4<-reactive({
       df <- read.csv(input$file4$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, agr = "constant") 
       ADFG <- st_read("Data/Alaska_Marine_Management_Areas.gdb")%>%
         filter(Area_Type=="ADFG Stat Area")
       df <- st_join(df, ADFG, join = st_within)
       df <-df%>% 
         data.frame%>%
         dplyr::select(!c(ID,Area_Type,AreaID,Area_Name,SourceURL,SourceName,BSIERP_ID,
                          BSIERP_Region_Name, Ecosystem_Subarea,Ecosystem_Area,NMFS_REP_AREA,
                          FISHERY_GR,REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, 
                          REGISTRA_3,DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                          SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                          INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_1,NMFS_REP_2,
                          IFQ_IPHC_A,IFQ_SABLEF,COAR_AREA_,Shape_Area,Shape_Length,geometry))
     })
     
     #BSIERP
     data5<-reactive({
       df <- read.csv(input$file5$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, agr = "constant") 
       BSIERP <- st_read("Data/Alaska_Marine_Management_Areas.gdb")%>%
        filter(Area_Type=="BSIERP Region")
       df <- st_join(df, BSIERP, join = st_within)
       df <-df%>% 
         data.frame%>%
         dplyr::select(!c(ID,Area_Type,AreaID,Area_Name,SourceURL,SourceName,
                          Ecosystem_Subarea,Ecosystem_Area,NMFS_REP_AREA,
                          STAT_AREA,FISHERY_GR,REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, 
                          REGISTRA_3,DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                          SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                          INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_1,NMFS_REP_2,
                          IFQ_IPHC_A,IFQ_SABLEF,COAR_AREA_,Shape_Area,Shape_Length,geometry))
     })
     
     #CRAB
     data6<-reactive({
       df <- read.csv(input$file6$datapath,
                      header = TRUE)%>%
         mutate(LAT=latitude, LON=longitude)
       df = st_as_sf(df, coords = c("LON", "LAT"), crs = 4269, agr = "constant")
       #upload crab shapefiles
       CRAB_SM<-st_read("Data/St_Matthew_District.shp")%>%
         mutate(CRAB_DISTRICT="Saint Matthew")
       CRAB_BB<-st_read("Data/BristolBay.shp")%>%
         mutate(CRAB_DISTRICT="Bristol Bay")
       CRAB<-bind_rows(CRAB_BB, CRAB_SM)
       #  Transform the points to match the bsierp regions projection
       df <- df %>% st_transform(st_crs(CRAB)$proj4string)
       df <- st_join(df, CRAB, join = st_within)
       df <-df%>% 
         data.frame%>%
         dplyr::select(!c(Id,X_Centroid, Y_Centroid,Shape_Area,Shape_Leng,geometry))
     })
     
    #ESR output
    output$ESR_DL<-downloadHandler(
    filename = function() {
      paste0(input$file1,"_ESR.csv")
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
        paste0(input$file2,"_LME.csv")
      },
      content = function(file) {
        write.csv(data2(), file)
      }
    )
    output$LME_table <- renderTable({
      req(input$file2)
      return(head(data2()))
    })
    
    #NMFS output
    output$NMFS_DL<-downloadHandler(
      filename = function() {
        paste0(input$file3,"_NMFS.csv")
      },
      content = function(file) {
        write.csv(data3(), file)
      }
    )
    output$NMFS_table <- renderTable({
      req(input$file3)
      return(head(data3()))
    })
    
    #ADFG output
    output$ADFG_DL<-downloadHandler(
      filename = function() {
        paste0(input$file4,"_ADFG.csv")
      },
      content = function(file) {
        write.csv(data4(), file)
      }
    )
    output$ADFG_table <- renderTable({
      req(input$file4)
      return(head(data4()))
    })
    
    #BSIERP output
    output$BSIERP_DL<-downloadHandler(
      filename = function() {
        paste0(input$file5,"_BSIERP.csv")
      },
      content = function(file) {
        write.csv(data5(), file)
      }
    )
    output$BSIERP_table <- renderTable({
      req(input$file5)
      return(head(data5()))
    })
    
    #CRAB output
    output$CRAB_DL<-downloadHandler(
      filename = function() {
        paste0(input$file6,"_CRAB.csv")
      },
      content = function(file) {
        write.csv(data6(), file)
      }
    )
    output$CRAB_table <- renderTable({
      req(input$file6)
      return(head(data6()))
    })
    
}
# Run the app ----
shinyApp(ui, server)

