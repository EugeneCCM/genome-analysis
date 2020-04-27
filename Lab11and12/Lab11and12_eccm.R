#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)


time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths")
time_series_recovered_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region") %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Recovered")
# Create Keys 
time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)
time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Deaths)
time_series_recovered_long <- time_series_recovered_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Recovered)
# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))
time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
    select(-Key)
# Reformat the data
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)
# Create Report table with counts
time_series_long_joined_counts <- time_series_long_joined %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date),
                 names_to = "Report_Type", values_to = "Counts")

# rename the data
global_time_series <- time_series_long_joined

# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)

# Define reporting types
Report_Type = c("Confirmed", "Deaths", "Recovered")

# Create list of countries
Countries = global_time_series$Country_Region

# Making a list of US and 4 other countries 

five_countries_ts <- global_time_series %>% filter(Country_Region %in% c("Malaysia","US","Spain","Italy","India"))
list_five_countries = five_countries_ts$Country_Region

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Lab 11/12"),
    p("Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
      tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
    ),
    tags$br(),
    tags$hr(),  # Adds line to page
    
    verticalLayout(
        sidebarLayout(
            sidebarPanel(
                
                # Select Reporting type
                selectInput("select_type", 
                            label = "Report Type", 
                            choices = "Confirmed", 
                            selected = "Confirmed"),
                # Date range
                dateRangeInput("dates", label = "Date range", start = first_date)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("Plot1")
            )
        )
    ),
    tags$hr(),
    sidebarLayout(
        sidebarPanel(
            
            # Select Reporting type
            selectInput("select_type_2",
                       label = "Report Type", 
                        choices = Report_Type, 
                        selected = "Confirmed"),
            # Date range
            dateRangeInput("dates", label = "Date range", start = first_date)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot2")
        )
    ),
    tags$hr(),
    sidebarLayout(
        sidebarPanel(
            # Select Country
            selectInput("select_country_global", 
                        label = "Country", 
                        choices = Countries),
            # Select Reporting type
            selectInput("select_type_global", 
                        label = "Report Type", 
                        choices = Report_Type, 
                        selected = "Confirmed"),
            # Date range
            dateRangeInput("dates", label = "Date range", start = first_date)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot3")
        )
    ),
    tags$hr(),
    
    sidebarLayout(
        sidebarPanel(
            # Select Reporting type
            selectInput("select_type", 
                        label = "Report Type", 
                        choices = Report_Type, selected = "Confirmed"),
            # Select Date 
            sliderInput("slider_date", label = "Report Date", min = first_date, 
                        max = last_date, value = first_date, step = 7)
        ),
        
        # Show a plots
        mainPanel(
            plotOutput("Plot4")
        )
    ),
    tags$hr(),

)


# Define server logic required to make the plot
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({
        
        # Note that aes_strings was used to accept y input and needed to quote other variables
        ggplot(five_countries_ts, aes_string(x = "Date",  y = "Confirmed", color = "Country_Region")) + 
            geom_point() +
            geom_line() +
            # Here is where the dates are used to set limits for x-axis
            xlim(input$dates) +
            ggtitle("Exercise 1:")
    })
    output$Plot2 <- renderPlot({
        pick_country <- global_time_series %>% 
            group_by(Country_Region,Date) %>% 
            summarise_at(c("Confirmed","Recovered","Deaths"), sum)
        # Note that aes_strings was used to accept y input and needed to quote other variables
        ggplot(five_countries_ts, aes_string(x = "Date",  y = input$select_type_2, color = "Country_Region")) + 
            geom_point() +
            geom_line() +
            # Here is where the dates are used to set limits for x-axis
            xlim(input$dates) +
            ggtitle("Exercise 2:", input$select_type_2)
    })
    output$Plot3 <- renderPlot({
        # Graph specific code
        pick_country_global <- global_time_series %>% 
            group_by(Country_Region,Date) %>% 
            summarise_at(c("Confirmed","Recovered","Deaths"), sum) %>% 
            # Here is where we select the country
            filter (Country_Region == input$select_country_global)
        
        # Note that aes_strings was used to accept y input and needed to quote other variables
        ggplot(pick_country_global, aes_string(x = "Date",  y = input$select_type_global, color = "Country_Region")) + 
            geom_point() +
            geom_line() +
            # Here is where the dates are used to set limits for x-axis
            xlim(input$dates) +
            ggtitle("Exercise 3:", input$select_type_global)
    })
    output$Plot4 <- renderPlot({
        # develop data set to graph
        pick_date <- global_time_series %>% 
            # Fix mapping to map_data of US != USA  
            mutate(Country_Region = recode(Country_Region, US = "USA")) %>% 
            # *** This is where the slider input with the date goes
            filter(Date == input$slider_date) %>% 
            group_by(Country_Region) %>% 
            summarise_at(c("Confirmed", "Deaths", "Recovered"), sum)
        
        # load the world map data
        world <- map_data("world")
        
        # We need to join the us map data with our daily report to make one data frame/tibble
        country_join <- left_join(world, pick_date, by = c("region" = "Country_Region"))
        
        # plot world map
        ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
            coord_fixed(1.5) + 
            # Add data layer
            geom_polygon(data = country_join, aes_string(fill = input$select_type), color = "black") +
            scale_fill_gradientn(colours = 
                                     wes_palette("Zissou1", 100, type = "continuous"),
                                 trans = "log10") +
            ggtitle("JHU COVID-19 data for reporting type:", input$select_type)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)