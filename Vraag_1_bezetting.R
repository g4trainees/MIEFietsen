###############################################################################
## Script data voor vragen fietsenstallingsgebruik
#
# Poolopdracht G4 Trainees Informatiemanagement (sep 2018- feb 2019)
# Scrumteam: Mobile Informatie Eenheid
# Door: Alma Raskadic, bewerkt door Jurriaan Duyne


#install.packages("readr")
#install.packages("dplyr")
library("readr")
library("dplyr")
#setwd("~/G4 traineepool/Data poolopdracht")

#vraag 1: Voldoende stalling? 
#vraag : Hoe vaak is de stalling vol? (>90%)? Wanneer is het vol?
#vraag 1B: Hoe vaak is de stalling bijna vol? (75%-90%)

#dataset openen en hernoemen
dataVoorbeeld <- read_csv("sstallingnet_lumiguide_stats Stadhuis 2018-06-01T00.00.00Z 2018-06-30T23.59.59Z 1h.csv", 
                                               col_types = cols(`time (UTC)` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))

verwerkBezetting <- function(dataNaam, locatieNaam){
  
  #Bezettingspercentages uitrekenen en nieuwe kolomn maken hiervoor. Aanname bezettingsgraagd is de gemiddelde bezetting gedeeld door de som van de gemiddelde bezetting plus de gemiddelde vrije plaatsen
  dataNaam$BezettingspercentageUUR <- (dataNaam$occupiedMean/(dataNaam$freeMean + dataNaam$occupiedMean))*100
  
  # percentage vrije stallingsplaatsen uitrekenen en kolom maken
  dataNaam$vrijpercentageUUR <- (dataNaam$freeMean/(dataNaam$freeMean + dataNaam$occupiedMean))*100
  
  #we ronden in beide nieuwe kolommen de percentages af op twee decimalen
  dataNaam$bezettingUUR <- round(dataNaam$BezettingspercentageUUR, digits = 2)
  dataNaam$vrijUUR <- round(dataNaam$vrijpercentageUUR, digits = 2)
  
  #we maken een dataframe met enkel de gemiddelde percentages
  Gemiddelde_bezetting <- data.frame(dataNaam$`time (UTC)`, dataNaam$BezettingspercentageUUR, dataNaam$vrijpercentageUUR)
  Gemiddelde_bezetting$Locatie <- locatieNaam
  colnames(Gemiddelde_bezetting) <- c("Tijd", "BezettingspercentageUur", "VrijpercentageUur", "Locatie")
  
  return(Gemiddelde_bezetting)
}

bezetting <- verwerkBezetting(dataVoorbeeld, "Stadhuis")

write.csv(bezetting, "C:/Users/Alma/Desktop/G4 traineepool/Data poolopdracht/vraag1nieuw.csv")

#Even ggplot erbij halen en installeren indien niet aanwezig. 
#install.packages("ggplot2")
library(ggplot2)
library(plotly)

#plot voor bezettingspercentage maken 

ggplot(bezetting, aes(x = `time (UTC)`, y = BezettingspercentageUUR)) + geom_line()+
  geom_point() +
  expand_limits(y=0) +
  ylab("% gemiddelde bezetting") + xlab("tijd") +
  ggtitle("Gemiddelde bezetting per uur in Juni")


interactieve_plot <- plot_ly(bezetting, x = ~`time (UTC)`, y = ~BezettingspercentageUUR, name = "Gemiddelde bezetting per uur", type = 'scatter', mode = 'lines',
        line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
  layout(title = "Aantal fietsen checkins en checkouts",
         xaxis = list(title = "Uren"),
         yaxis = list (title = "% gemiddelde bezetting per uur"))
interactieve_plot
