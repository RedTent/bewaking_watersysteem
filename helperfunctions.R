import_data <- function(datacsv="fys_chem.csv"){
  require(readr)
  require(dplyr)
  require(lubridate)
  #kolomnamen <- c("mp","datum","parnr","par","eenheid","detectiegrens","waarde")
  df <- read_csv2(file=datacsv,col_types=cols(datum=col_date(format="%d-%m-%Y %H:%M:%S")))
  df <- dplyr::filter(df,!is.na(waarde)) # alle metingen moeten een meetwaarde hebben
  #Toevoegen jaren en maanden
  df$jaar <-as.integer(year(df$datum))
  df$maand <- as.integer(month(df$datum))
  
  #info zodat je weet wat je importeert
  print(paste("Laatste meetdatum is",max(df$datum)))
  df
}

import_meetpunten <- function(meetpuntencsv="meetpunten.csv"){
  require(dplyr)
  require(readr)
  meetpuntendf <- read_csv2(meetpuntencsv,col_types = cols())
  names(meetpuntendf) <- tolower(names(meetpuntendf))
  meetpuntendf <- meetpuntendf %>% rename(X=x,Y=y)
  meetpuntendf
}

import_meetpunten_latlong <- function(meetpuntencsv="meetpunten.csv", X = "X", Y = "Y"){
  require(rgdal)
  require(dplyr)
  meetpuntendf <- import_meetpunten(meetpuntencsv)
  longlat <- meetpuntendf %>% filter(X != 0, Y != 0) %>%  mutate(long = X, lat = Y)
  coordinates(longlat) = ~long+lat
  proj4string(longlat) <- CRS("+init=EPSG:28992")
  longlat <- spTransform(longlat,"+init=EPSG:4326")
  meetpuntendf <- left_join(meetpuntendf, select(as.data.frame(longlat), mp, long, lat), by = "mp")
}

import_parameters <- function(parametercsv='parameters.csv'){
  require(readr)
  parameterdf <-read_csv2(parametercsv, col_types = cols())
  parameterdf
}

df_to_named_list <- function(df, waarden=1, namen=2){
  require(dplyr)
  values <- c(dplyr::select(df, waarden), use.names = FALSE, recursive = TRUE)
  names_values <- c(dplyr::select(df, namen), use.names = FALSE, recursive = TRUE)
  names(values) <- names_values
  values
}

HHSKthema <- function(){
  require(ggplot2)
  hhskgroen <<- "#8dc63f"
  hhskblauw <<- "#0079c2"
  hhskthema <<- theme_light() + 
    theme( plot.title = element_text(color = hhskgroen, face = "bold", hjust = 0.5),
           axis.title = element_text(color = hhskblauw, face = "bold"),
           axis.text = element_text(color = hhskblauw),
           axis.ticks = element_line(color = hhskblauw),
           axis.line.x = element_line(color = hhskblauw, size = 0.5),
           panel.border = element_rect(color = hhskblauw, size = 1),
           panel.grid.major = element_line(color = hhskgroen,linetype = "dotted", size = 0.5),
           panel.grid.minor = element_line(color = hhskgroen,linetype = "dotted", size = 0.5),
           strip.background = element_rect(size = 20, fill = "white"),
           strip.text = element_text(color = hhskgroen, size = 14, face = "bold")
           
    )
}

meetpuntinformatie <- function(mpcode="00016", meetpuntendf){
  mpomsch <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"mpomsch"]
  X <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"X"]
  Y <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"Y"]
  gebied <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"gebied"]
  meetnet <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"meetnet"]
  meetpuntsoort <- meetpuntendf[meetpuntendf[["mp"]]==mpcode,"meetpuntsoort"]
  return(list("mpcode"=mpcode,"mpomsch"=mpomsch,"X"=X,"Y"=Y,"gebied"=gebied,"meetnet"=meetnet,"meetpuntsoort"=meetpuntsoort))
}#end of function

parameterinformatie <- function(parnr="3",parameterdf){
  parameterkort <- as.character(parameterdf[parameterdf[["parnr"]]==parnr,"par"])
  parameterlang <-  as.character(parameterdf[parameterdf[["parnr"]]==parnr,"parnaamlang"])
  eenheid <-  as.character(parameterdf[parameterdf[["parnr"]]==parnr,"eenheid"])
  return(list("parnr"=parnr,"parameterkort"=parameterkort,"parameterlang"=parameterlang,"eenheid"=eenheid))
}#end of function

tijdreeksgrafiek <- function(df, meetpunt, parameternr, parameterdf,meetpuntendf){
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  
  HHSKthema()
  
  mpinfo <- meetpuntinformatie(meetpunt,meetpuntendf=meetpuntendf)
  paraminfo <- parameterinformatie(parnr = parameternr, parameterdf = parameterdf)
  min <- min(df$waarde)
  max <- max(df$waarde)
  if(min/(max-min)>1){ylimieten <- c(min*0.9,max*1.1)}else {ylimieten <- c(0,max*1.1)}
  
  #GGPLOT opjecten opbouwen
  lijn <- geom_line(col=hhskblauw)
  punten <- geom_point(col=hhskblauw)
  grafiektitel <- ggtitle(paste("Meetpunt:",meetpunt,"-",mpinfo$mpomsch,"\n","Parameter:",paraminfo$parameterlang))
  y_label <- ylab(paraminfo$eenheid)
  x_label <- xlab("")
  x_axis <- scale_x_date(date_breaks = "years",labels=year)
  y_axis <- scale_y_continuous(limits=ylimieten,expand=c(0,0),oob=scales::rescale_none)#oob makes sure that the CI for LOESS is always plotted
  loess_lijn <- geom_smooth(se=TRUE,col=hhskgroen,linetype="dashed",fill=hhskblauw,alpha=0.08,fullrange=TRUE)
  
  plot <- ggplot(df, aes(x=datum,y=waarde))+lijn+punten+grafiektitel+x_label+y_label+x_axis+y_axis+loess_lijn+hhskthema
  plot
  
}#end of function

