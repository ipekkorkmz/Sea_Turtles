
#    http://shiny.rstudio.com/

# Importing libraries
library(readr)
library(dplyr)
library(shiny)
library(DT)
library(plotly)
library(sf)
library(leaflet)
library(htmltools)

# Importing datasets
data <- read_csv("~/Sea_Turtles/data/data.csv")
yearly_data <- read_csv("~/Sea_Turtles/data/yearly_data.csv")

data_sf <- st_read("~/Sea_Turtles/data/data_sf.shp")
colnames(data_sf) <- c("code", "country", "siteid", "site_name","compiler", "species",
                    "common_name", "years_monitored", "nesting_status", "geometry")

# Creating colors vector
colors = c("Caretta caretta" = "#7FC97F",
           "Chelonia mydas" = "#BEAED4",
           "Dermochelys coriacea" = "#FDC086",
           "Lepidochelys olivacea" = "#E8E46E",
           "Eretmochelys imbricata" = "#386CB0",
           "Natator depressus" = "#f29191",
           "Lepidochelys kempii" = "#999b84")

ui <- fluidPage(

    # Title
    titlePanel("Observation of Sea Turtles Over the Years"),

    
    sidebarLayout(
        sidebarPanel(
            
        # Checkbox for each sea turtle species
        checkboxGroupInput(inputId = "selected_species",
                           label = h3("Species"), 
                           choices = unique(data$species),
                           selected = "Caretta caretta"),
        
        # Slider range to change range of observed years (x-axis)
        sliderInput(inputId = "slider_range",
                   label = h3("Observed Years"),
                    min = 1929, max = 2019,
                    value = c(1970, 2019),
                    sep = ""), #to not be separating with a comma
        
        # Slider to change numbers of observation (y-axis)
        sliderInput("slider", label = h3("Numbers of Observation"),
                    min = 0, max = 1250, value = 500),
        
        # Single checkbox for addinonal information
        checkboxInput(inputId = "summary", label = "Information", value = FALSE),
        
        # Single checkbox for showing table
        checkboxInput(inputId = "show_table", label = "Show table for map",
                      value = TRUE)
        
        ),
        
        mainPanel(
            tabsetPanel(
                # First panel
                tabPanel("Bar Graph",
                         plotlyOutput("BarPlot"), verbatimTextOutput("Info")),
                # Second panel
                tabPanel("Line Graph",
                         plotlyOutput("LinePlot"), br(),br(),br(), plotlyOutput("PieChart")), 
                # Third panel
                tabPanel("Map", 
                         leafletOutput("seaturtlemap"), br(),br(),
                         column(dataTableOutput("group_summary"), width = 6)),
                # Fourth panel
                tabPanel("Table",
                         dataTableOutput("seaturtle_table"))
            )
        )
        
        
    )
)


server <- function(input, output) {
    
    
    # Reactive data set which contains species selected by users
    seaturtle_subset <- reactive({
         yearly_data %>% 
             filter(species %in% input$selected_species)
    })
    
    # Information for first panel
    output$Info <- renderPrint({
        if(input$summary){
            
            first_year <- seaturtle_subset() %>%
                        select(years_monitored) %>%
                        min()
            
            last_year <- seaturtle_subset() %>%
                        select(years_monitored) %>%
                        max()
            
            max_obsi <- seaturtle_subset() %>%
                        ungroup() %>% #to not add years_monitored column
                        select(n) %>%
                        max()
            
            cat(sprintf("First observation: %d \n", first_year))
            cat(sprintf("Last observation: %d \n", last_year))
            cat(sprintf("Maximum observation: %d \n", max_obsi))
        }
    })
    
    
    # Interactive bar chart
    output$BarPlot <- renderPlotly({
        
        barchar <- seaturtle_subset() %>%
                ggplot(aes(x = years_monitored, y = n, fill = species)) +
                geom_bar(stat = "identity", position = "stack") +
                labs(y = "Number of Observations",
                    x = ("Observed Years"),
                    title = "Species' numbers over the years",
                    fill = "Species") +
                xlim(input$slider_range) +
                ylim(0, input$slider) +
                theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                    legend.title = element_text(face = "bold"),
                    axis.title = element_text(face = "bold"),
                    panel.background = element_rect("#Fcfeff")) +
                scale_fill_manual(values = colors)
        
        ggplotly(barchar,
                 tooltip = c("x", "y"))
    })
    
    
    # Interactive line graph
    output$LinePlot <- renderPlotly({
        
        linechar <- seaturtle_subset() %>%
                    ggplot( aes(x=years_monitored, y=n, group=species,
                            color=species)) +
                    geom_line(size = 1) +
                    geom_point(shape=19, size=1.5) +
                    labs(y = "Number of Observations", x = ("Observed Years"),
                        title = "Species' numbers over the years") +
                    xlim(input$slider_range) +
                    ylim(0, input$slider) +
                    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                        legend.title = element_text(face = "bold"),
                        axis.title = element_text(face = "bold"),
                        panel.background = element_rect("#Fcfeff")) +
                    scale_colour_manual("Species", values = colors)
        
        ggplotly(linechar,
                 tooltip = c("x", "y"))
        
    })
    
    
    # Reactive data set for pie chart
    seaturtle_count <- reactive({
        data %>% 
            filter(species %in% input$selected_species &
                   years_monitored >= input$slider_range[1] &
                   years_monitored <= input$slider_range[2]) %>%
            group_by(species) %>%
            summarize(n = n())
    })
    
    
    # Interactive pie chart
    output$PieChart <- renderPlotly({
        pie_title <- sprintf("<b style='color: black'>Total observation of species between %d and %d</b>",
                             input$slider_range[1], input$slider_range[2])
        seaturtle_count() %>%
        plot_ly(labels = ~species, values = ~n, type = 'pie',
                marker = list(colors = case_when(seaturtle_count()$species == "Caretta caretta" ~ "#7FC97F",
                                                 seaturtle_count()$species == "Chelonia mydas" ~ "#BEAED4",
                                                 seaturtle_count()$species == "Dermochelys coriacea" ~ "#FDC086",
                                                 seaturtle_count()$species == "Lepidochelys olivacea" ~ "#E8E46E",
                                                 seaturtle_count()$species == "Eretmochelys imbricata" ~ "#386CB0",
                                                 seaturtle_count()$species == "Natator depressus" ~ "#f29191",
                                                 seaturtle_count()$species == "Lepidochelys kempii" ~ "#999b84"),
                              line = list(color = '#FFFFFF', width = 1))) %>%
                layout(title = pie_title, font = list(face="bold"),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       margin = list(t=50))
    })
    

    # Reactive data set for marking map    
    seaturtlemap_subset <- reactive({
        data_sf %>% 
            filter(species %in% input$selected_species &
                       years_monitored >= input$slider_range[1] &
                       years_monitored <= input$slider_range[2])
    })
    
    # Interactive map    
    output$seaturtlemap <- renderLeaflet({
        pal <- colorFactor(colors, domain = data$species)
        labels <-   sprintf("<strong>%s</strong><br/>
                            Site Name: %s<br/>
                            Observed year: %s",
                            seaturtlemap_subset()$species,
                            seaturtlemap_subset()$site_name,
                            seaturtlemap_subset()$years_monitored) %>%
            lapply(htmltools::HTML)
        
        seaturtlemap_subset() %>%
            leaflet() %>%
            addTiles() %>%
            addCircleMarkers(radius = 5,
                             color = ~pal(species),
                             stroke = FALSE,
                             fillOpacity = 1,
                             label = labels) %>%
            addLegend("bottomright",
                      pal=pal,
                      values=~species,
                      title = 'Species')
    })
    
    # Information for map
    output$group_summary <- renderDataTable({
        if(input$show_table){
            seaturtlemap_subset() %>%
                st_drop_geometry() %>%
                group_by(species) %>%
                summarize(Count = n())
        }
    })    
    
    # All data set
    output$seaturtle_table <- renderDataTable(
        data %>%
            select(-compiler, -nesting_status)
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
