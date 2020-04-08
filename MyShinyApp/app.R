#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)


data_url <- "https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A35ad7624-b159-4e29-a700-0c0770419941"
bg_chem <- read.csv(url(data_url, method = "libcurl"), stringsAsFactors = FALSE) %>%
    select(-Date, -Time, -Station)
cols<- names(bg_chem)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Water Biogeochemistry"),
    p("Data for this application are from: "),
    tags$ul(
        tags$li("Craig Tweedie. 2009. North Pole Environmental Observatory Bottle Chemistry. Arctic Data Center.",
                tags$a("doi:10.18739/A25T3FZ8X", href="http://doi.org/10.18739/A25T3FZ8X")
        )
    ),
    tags$br(),
    tags$hr(),
    

    # Sidebar with a slider input for number of bins 
    
    verticalLayout(
        # Sidebar with a slider input for depth axis
        sidebarLayout(
            sidebarPanel(
                sliderInput("depth",
                            "Depth:",
                            min = 1,
                            max = 500,
                            value = c(1, 100))
            ),
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("depthPlot")
            )
        ),
        
        tags$hr(),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("x_variable", "X Variable", cols, selected = "CTD_Salinity"),
                selectInput("y_variable", "Y Variable", cols, selected = "d18O"),
                selectInput("color_variable", "Color", cols, selected = "P")
            ),
            
            # Show a plot with configurable axes
            mainPanel(
                plotOutput("varPlot")
            )
        )
),
        tags$hr()

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$depthPlot <- renderPlot({
        
        ggplot(bg_chem, mapping = aes(CTD_Depth, CTD_Salinity)) +
            geom_point(colour="red", size=4) +
            xlim(input$depth[1],input$depth[2]) +
            theme_light()
        
    })
    output$varPlot <- renderPlot({
        ggplot(bg_chem, mapping = aes_string(x = input$x_variable,
                                             y = input$y_variable,
                                             color = input$color_variable)) +
            geom_point(size=4) +
            scale_color_gradient2(low="midnightblue", 
                                  mid="white", 
                                  high="firebrick", 
                                  midpoint = mean(bg_chem[,input$color_variable])) +
            theme_light()
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

