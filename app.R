
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(DT)


meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv") 
basis_meetpunten <- meetpuntendf %>% filter(`basis-jaarlijks` == 1) %>% select(mp, `landgebruik 2015`)
data <- import_data("data/fys_chem.zip") %>% semi_join(y= basis_meetpunten, by = "mp") %>% left_join(y = select(meetpuntendf, mp, `landgebruik 2015`), by = "mp")
data <- data %>% group_by(mp, parnr, par, jaar, maand) %>% summarise(maand_gem_waarde = mean(waarde)) %>% ungroup() %>% filter(parnr < 18)
aantal_vgl_jaren <- 10

### onderbrengen in server
#jaar_keuze <- 2017
#maand_keuze <- 6
#data_sel <- data %>% filter(jaar > jaar_keuze-10)
#data2 <- data_sel %>% filter(jaar > jaar_keuze-10) %>% group_by(mp, parnr, par, jaar, maand) %>% summarise(maand_gem_waarde = mean(waarde)) %>% ungroup() %>% 
#  group_by(mp, parnr, maand) %>% mutate(rank = rank(maand_gem_waarde), aantal = n(), norm_rank = 9*(rank-1)/(aantal-1)+1) %>% ungroup()
#data_cl <- data2 %>% filter(jaar == jaar_keuze, maand == maand_keuze, parnr ==1) 
#data_cl
#hist(data_cl$norm_rank)



#data_sel <- filter(data, jaar == jaar_keuze, maand == maand_keuze) %>% group_by(parnr, par) %>% summarise(average_rank = mean(rank))
#data_sel

#data$rank <- group_indices(data, mp, parnr, maand)

# UI---------
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("jaar_keuze", "Kies jaar", min = min(data$jaar)+10, max = max(data$jaar), value = max(data$jaar), step = 1, sep = "" ),
         sliderInput("maand_keuze", "Kies maand", min = 1, max = 12, value = 1, step = 1, sep = "" )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("ranked_data")
      )
   )
)


# SERVER ------------------------
server <- function(input, output) {
   
    ranked_data <- reactive({
      data <- filter(data, maand == input$maand_keuze, jaar > (input$jaar_keuze - aantal_vgl_jaren), jaar <= input$jaar_keuze )
      ranked_data <-  data %>% 
                      group_by(mp, parnr, maand) %>% 
                      mutate(rank = rank(maand_gem_waarde), aantal = n(), norm_rank = (aantal_vgl_jaren-1)*(rank-1)/(aantal-1)+1, rel_rank = (rank-1)/(aantal-1)) %>% 
                      ungroup() %>% 
                      filter(jaar == input$jaar_keuze)
      
    })  
  
   output$ranked_data <- renderDataTable({
     summary <- ranked_data() %>% group_by(parnr, par) %>% summarise(gem_rank = round(mean(norm_rank, na.rm = TRUE), digits=1), perc_rank = round(mean(rel_rank, na.rm = TRUE)*100, digits=0))
     datatable(summary)
     }) # end output ranked_data
   
}#end server

# Run the application ------
shinyApp(ui = ui, server = server)

