#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("helperfunctions.R")

library(shiny)
library(dplyr)


meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv") 
basis_meetpunten <- meetpuntendf %>% filter(`basis-jaarlijks` == 1) %>% select(mp, `landgebruik 2015`)
data <- import_data("data/fys_chem.zip") %>% semi_join(y= basis_meetpunten, by = "mp") %>% left_join(y = select(meetpuntendf, mp, `landgebruik 2015`), by = "mp")

jaar_keuze <- 2017
maand_keuze <- 6
data <- data %>% filter(jaar > jaar_keuze-10)
data2 <- data %>% filter(jaar > jaar_keuze-10) %>% group_by(mp, parnr, par, jaar, maand) %>% summarise(maand_gem_waarde = mean(waarde)) %>% ungroup() %>% 
  group_by(mp, parnr, maand) %>% mutate(rank = rank(maand_gem_waarde), aantal = n(), norm_rank = 10*rank/aantal) %>% ungroup()
data_cl <- data2 %>% filter(jaar == jaar_keuze, maand == maand_keuze, parnr ==1) 
data_cl
hist(data_cl$norm_rank)

#data_sel <- filter(data, jaar == jaar_keuze, maand == maand_keuze) %>% group_by(parnr, par) %>% summarise(average_rank = mean(rank))
#data_sel

#data$rank <- group_indices(data, mp, parnr, maand)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

