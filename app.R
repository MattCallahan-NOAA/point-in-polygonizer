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
  tags$blockquote("This tool accepts latitude and longitude coordinates and adds the ADFG statistical area, nmfs area, and IPHC area. These spatial strata are  relevant to fisheries scientists and managers. All three spatial bounds were calculated from an ADFG stat area shapefile using the WGS 1984 coordinate system"),
  tags$blockquote("To use this tool, simply submit a csv with coordinates using the 'Browse' button. Coodinate fields must be titled 'latitude' and 'longitude' (lowercase), and the coordinate format must be in decimal degrees."),
  
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
      downloadButton("contents","Download updated data")),

    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents2")
      
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
 
     data<-reactive({
    #read in points
    df <- read.csv(input$file1$datapath,
                   header = TRUE)%>%
      mutate(LAT=latitude, LON=longitude)
    
    #  define as shapefile and set a crs of WGS84
    df = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") 
    #read in shapefile
    adfg <- st_read('Data/adfg_stat_areas_simple.shp')
    
    
    #  Transform the points to match the adfg  projection
   df <- df %>% st_transform(st_crs(adfg)$proj4string)
    #  Match the poiints to areas
    df <- st_join(df, adfg, join = st_within)
    #convert back to data frame
    df <-df%>% 
      data.frame%>%
      dplyr::select(!c(OBJECTID,GEOMETRY_S,GEOMETRY_E,FISHERY_GR,GIS_SERIES,GIS_SERI_1,
                       REGION_COD, REGISTRATI,REGISTRA_1, REGISTRA_2, REGISTRA_3, 
                       DISTRICT_N,DISTRICT_C,DISTRICT_I,SUBDISTRIC,SUBDISTR_1,
                       SUBDISTR_2,SECTION_NA,SECTION_CO,SECTION_ID,SECTOR_NAM,SECTOR_COD,
                       INSIDE_OUT,WATERS_COD,FMP_AREA_C,NMFS_REPOR,NMFS_REP_2,
                       IFQ_SABLEF,COAR_AREA_,CREATOR,CREATE_DAT,EDITOR,   
                       EDIT_DATE,COMMENTS,STAT_AREA_,Shape__Are,Shape__Len,geometry))

     })  
    
    output$contents<-downloadHandler(
    filename = function() {
      paste0(input$file1,"_polygonized.csv")
      },
    content = function(file) {
      write.csv(data(), file)
      }
    )
  
    output$contents2 <- renderTable({
      req(input$file1)
      return(head(data()))
    })

}
# Run the app ----
shinyApp(ui, server)

