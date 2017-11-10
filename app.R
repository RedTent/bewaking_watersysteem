
source("helperfunctions.R")

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)

aantal_vgl_jaren <- 10

HHSKthema()

meetpuntendf <- import_meetpunten_latlong("data/meetpunten.csv") 
basis_meetpunten <- meetpuntendf %>% filter(`basis-jaarlijks` == 1) %>% select(mp, `landgebruik 2015`)
data <- import_data("data/fys_chem.zip") %>% semi_join(y= basis_meetpunten, by = "mp") %>% left_join(y = select(meetpuntendf, mp, `landgebruik 2015`), by = "mp")
data <- data %>% group_by(mp, parnr, par, jaar, maand, `landgebruik 2015`) %>% summarise(maand_gem_waarde = mean(waarde)) %>% ungroup() %>% filter(parnr %in% c(1,3,7,8,9,10,12,13,14,15))



# UI---------
ui <- fluidPage(

   sidebarLayout(
      sidebarPanel(h1("Bewaking watersysteem"), HTML("</br>"),
         sliderInput("jaar_keuze", "Kies jaar", min = min(data$jaar)+10, max = max(data$jaar), value = max(data$jaar), step = 1, sep = "" ),
         sliderInput("maand_keuze", "Kies maand", min = 1, max = 12, value = 1, step = 1, sep = ""),
         radioButtons("meetpuntgroep", "Kies groep", choiceNames = c("Alle basis meetpunten", "Boezems", "Boezems en uitwisselpunten", "Meren en plassen"), choiceValues = c(1,2,3,4))
      ), # end of sidebar
      
      mainPanel(
        plotOutput("hists", height = 500),
        dataTableOutput("ranked_data")
      ) # end of main panel
   )
)


# SERVER ------------------------
server <- function(input, output) {

    data_sel <- reactive({
      if(input$meetpuntgroep == 1){data <- data}
      if(input$meetpuntgroep == 2){data <- filter(data, `landgebruik 2015` == "Boezem") }
      if(input$meetpuntgroep == 3){data <- filter(data, `landgebruik 2015` %in% c("Boezem", "Afvoer/gemaal")) }
      if(input$meetpuntgroep == 4){data <- filter(data, `landgebruik 2015` == "Plassen") }
      data <- data %>% filter( maand == input$maand_keuze, jaar > (input$jaar_keuze - aantal_vgl_jaren), jaar <= input$jaar_keuze )
      data
    }) #end data_sel
     
    ranked_data <- reactive({
      ranked_data <-  data_sel() %>% 
                      arrange(mp, parnr, jaar) %>% 
                      group_by(mp, parnr, maand) %>% 
                      mutate(rank = rank(maand_gem_waarde), aantal = n(), norm_rank = (aantal_vgl_jaren-1)*(rank-1)/(aantal-1)+1, perc_rank = (rank-1)/(aantal-1)*100 ) %>% 
                      mutate(gem_excl = mean(maand_gem_waarde[-aantal], na.rm = TRUE),
                             sd_excl = sd(maand_gem_waarde[-aantal], na.rm = TRUE),
                             check = maand_gem_waarde[aantal]) %>% #filter(parnr == 8) %>% print() %>% 
                      ungroup() %>% 
                      filter(jaar == input$jaar_keuze) %>% 
                      rowwise() %>% 
                      mutate(norm_waarde = (maand_gem_waarde-gem_excl)/sd_excl) %>% 
                      mutate(norm_waarde = case_when(abs(norm_waarde) == Inf ~ 0, TRUE ~ norm_waarde)) %>% 
                      ungroup() 
    })  #end ranked_data()
  
   output$ranked_data <- renderDataTable({
     summary <- ranked_data() %>% group_by(parnr, par) %>% 
       summarise(gem_rank = round(mean(norm_rank, na.rm = TRUE), digits = 1), 
                 tot_metingen = sum(aantal), 
                 aantal_in_meetjaar = n(), 
                 gem_perc_rank = round(mean(perc_rank, na.rm = TRUE), digits = 0),
                 gem_norm_waarde = round(mean(norm_waarde, na.rm = TRUE), digits = 2)) 
     datatable(summary)
     }) # end output ranked_data
   
   output$hists <- renderPlot({
     
     plots <- ranked_data() %>% mutate (par_combi = if_else(parnr<10, paste0("0",parnr," ", par), paste(parnr,par)) ) %>% 
       ggplot(aes(x = perc_rank)) + geom_histogram(fill = hhskblauw, bins = 10) + hhskthema + facet_wrap(~par_combi, ncol=5)
     plots
   })
   
   
}#end server

# Run the application ------
shinyApp(ui = ui, server = server)

