library(shiny)
library(leaflet)
library(sf)
library(here)
library(readxl)
library(tidyverse)

working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

all_by_id = read.csv(here('Outputs','PR', 'all_by_id.csv'))
groups = read_excel(here('Lookups', 'SpeciesGroups_DRAFT.xlsx'))
group_options = names(groups)[2:ncol(groups)]



# bring in the block shapefile from lookup folder
blocks_org <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013_clip.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks_org = st_transform(blocks_org, crs = 4326) 
blocks = select(blocks_org, NM_INDEX)
catch_choices = unique(all_by_id$Common_Name_catch)
effort_choices = unique(all_by_id$TripType_Description_effort)

# Define the UI
ui <- fluidPage(
  titlePanel("CRFS Mapping"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('samples', 'Minimum Number of Samples', choices = c(1,2,3,4,5,6,7,8,9,10), selected = 1),
      selectInput("catch_species", "Catch Species", choices = c('All', group_options, catch_choices), selected = 'Rockfish'),
      selectInput("trip_type", "Trip Type", choices = c('All', effort_choices ), selected = 'Bottomfish'),
      sliderInput('year', 'Year', min(all_by_id$year), max(all_by_id$year), value = range(2021, 2022), step=1, sep=""),
      textInput('zoom', 'Zoom To'),
      actionButton('ZoomButton', 'Zoom to')
    ),
    
    mainPanel(
      textOutput('text'),
      leafletOutput("map"),
      dataTableOutput('table')))
  )

# Define the server logic
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -122.85, lat = 37.45, zoom = '7')
  })
  
    # Render the map
    

    observe({
      
      catch = input$catch_species
      effort = input$trip_type
      min_year_selected = input$year[1]
      max_year_selected = input$year[2]
      samples = input$samples
      
      
      # Filter the dataframe based on user inputs
      
      if (catch == 'All'){
        filtered_catch = all_by_id %>%
          mutate(Common_Name_catch = 'All') %>%
          filter(year >= min_year_selected & year <= max_year_selected ) %>%
          mutate(year = ifelse(min_year_selected == max_year_selected, min_year_selected, paste0(min_year_selected, " - ", max_year_selected)))

      } else if (catch %in% group_options) {
                
        species = filter(groups, !!as.symbol(catch) == 1)
        filtered_catch = all_by_id %>%
          filter(Common_Name_catch %in% species$Common_Name_catch) %>%
          mutate(Common_Name_catch = catch ) %>%
          filter(year >= min_year_selected & year <= max_year_selected ) %>%
          mutate(year = ifelse(min_year_selected == max_year_selected, min_year_selected, paste0(min_year_selected, " - ", max_year_selected)))
        
      } else {
        
        filtered_catch = all_by_id %>%
          filter(Common_Name_catch == catch) %>%
          filter(year >= min_year_selected & year <= max_year_selected ) %>%
          mutate(year = ifelse(min_year_selected == max_year_selected, min_year_selected, paste0(min_year_selected, " - ", max_year_selected)))

      }

      if (effort == 'All') {
        filtered_effort = all_by_id %>%
          filter(year >= min_year_selected & year <= max_year_selected ) %>%
          mutate(TripType_Description_effort = 'All') %>%
          filter(year >= min_year_selected & year <= max_year_selected ) %>%
          mutate(year = ifelse(min_year_selected == max_year_selected, min_year_selected, paste0(min_year_selected, " - ", max_year_selected))) 
        
          
      } else {
      filtered_effort = all_by_id %>%
        filter(TripType_Description_effort == effort) %>%
        filter(year >= min_year_selected & year <= max_year_selected ) %>%
        mutate(year = ifelse(min_year_selected == max_year_selected, min_year_selected, paste0(min_year_selected, " - ", max_year_selected)))
      
     
      }
      

      
      if (effort == 'All'){
        
        filtered_catch_witheffort = filtered_catch %>%
          mutate(TripType_Description_effort = 'All')
      } else {
        filtered_catch_witheffort = filter(filtered_catch, TripType_Description_effort == effort)
        
      }
      
      # due to a given id having multiple rows for catching multiple species the total angler days per ID needs to be summed
      filtered_effort = filtered_effort %>%
        select(id, year, Block, Vessels, AnglerDays, TripType_Description_effort) %>%
        unique()
      
     
     aggregate_effort = filtered_effort %>%
       group_by(Block, TripType_Description_effort, year) %>%
       summarise(AnglerDays = sum(AnglerDays, na.rm = T),
                 n_samples_effort = n_distinct(id))
      
     aggregate_catch = filtered_catch_witheffort %>%
        group_by(Block, Common_Name_catch, TripType_Description_effort, year) %>%
        summarise(TotalCatch = sum(Total_Fish_Caught, na.rm = T),
                  Kept = round(sum(Total_Obs_Fish_Caught, na.rm = T), 2),
                  Released = round(sum(Total_Rep_Fish_Caught, na.rm=T), 2)) 
     
     joined = aggregate_effort %>%
        left_join(aggregate_catch, by = c("Block", "year", "TripType_Description_effort")) %>%
        mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
        mutate(CPUA_Kept = round(Kept/AnglerDays, 3))
     

      sample_limited = joined %>%
        filter(n_samples_effort >= samples) %>%
        select(Block, year, Species = Common_Name_catch, Trip =TripType_Description_effort, CPUA_All, CPUA_Kept, TotalCatch, Kept, Released, AnglerDays, Samples = n_samples_effort)
      
      sample_limited_table = sample_limited %>%
        mutate_at(c('CPUA_All','CPUA_Kept', 'TotalCatch', 'Kept', 'Released'), ~replace_na(.,0))
      
      if (nrow(sample_limited_table) == 0) {
        output$text = renderText('No results')
        leafletProxy("map") %>%
          clearShapes()
        output$table = renderTable(sample_limited_table)
        
        
      } else {

        layer = blocks %>%
          inner_join(sample_limited, by = c( 'NM_INDEX' = 'Block'))
      
        output$table = renderDataTable(sample_limited_table, options = list(
          pageLength = 10
          #columnDefs=list(list(targets=1:11, className = 'dt-center'))
          ))
        

      
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(data=layer,
                      
                      color = "#444444", 
                      stroke=FALSE,
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = ~colorNumeric('Reds', domain =layer$CPUA_Kept, na.color = 'transparent')(layer$CPUA_Kept),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup=paste0('Block:', layer$NM_INDEX, '<br>', 
                                   'CPUA: ', round(layer$CPUA_Kept,2), '<br>',
                                   'Samples: ', layer$Samples, '<br>'))
      }
    })
    
    observeEvent(input$ZoomButton, {
      
      selected_block = input$zoom
      zoom_block = filter(blocks_org, NM_INDEX == trimws(selected_block))

      if (nrow(zoom_block) == 1) {
      leafletProxy("map") %>%
        setView(lng = zoom_block$X[[1]], lat = zoom_block$Y[[1]], zoom = '12')      
      } else{
        showModal(modalDialog(
          title = "Block not found in lookup",
          "The block number is not found in the shapefile.",
          easyClose = TRUE
        ))
        }
        })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)