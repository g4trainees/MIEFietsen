###############################################################################
## Script data voor vragen fietsenstallingsgebruik
#
# Poolopdracht G4 Trainees Informatiemanagement (sep 2018- feb 2019)
# Scrumteam: Mobile Informatie Eenheid
# Door: Pieter Goethart, bewerkt door Jurriaan Duyne

## Vraag 2: 
# a) Hoe lang staan fietsen gestald per stalling? 
# b) Hoeveel procent staat er kort dan een uur?
# c) Hoeveel staat er tussen de 1 en de 4 uur?
# d) Hoeveel staat er tussen de 4 en de 12 uur?
# e) Hoeveel staat er tussen de 12 en de 24 uur?
# f) Hoeveel staat er langer dan 24 uur?
# g) Wisselt dit door de week heen?

## Uitleg logica: data op regeleniveau met check in en check out. Hier berekenen we hoe lang elke fiets gestald is geweest. Vervolgens wordt de checkout dag genomen voor de statistiek.

## Omgeving selecteren 
#setwd(~/G4 Traineeship/Poolopdrachten/3 Mobiele Informatie Eenheid Scrum/Scrum MIE Shared/Rwork PJG")
# voor het voorbeeld moet je de map slecteren met "FMS_2018_06_Utrecht_Stadhuis_ruwedata_transacties.csv"

## Packages laden (eventueel install.packages('PACKAGE_NAME'))

library(readr)
library(dplyr)
library(plotly)


############
##Inlezen databestanden 
# Afgesproken in scrumteam om fietsdata van het stadhuis te selecteren als begin, daarna uit te bouwen

# FMS ruwe data transacties
dataVoorbeeld <- read_delim("FMS_2018_06_Utrecht_Stadhuis_ruwedata_transacties.csv", 
                                   ";", escape_double = FALSE, col_types = cols(`Check-in datum` = col_date(format = "%d-%m-%y"), 
                                                                                `Check-in tijd` = col_time(format = "%H:%M:%S"), 
                                                                                `Check-out datum` = col_date(format = "%d-%m-%y"), 
                                                                                `Check-out tijd` = col_time(format = "%H:%M:%S")), 
                                   trim_ws = TRUE)


verwerkStallingsduur <- function(dataNaam, locatieNaam){
   
  ## Data voorbewerken
  dataNaam$Weekdag <- weekdays(dataNaam$`Check-out datum`)
  dataNaam$Weekdag <-ordered(dataNaam$Weekdag, levels=c("maandag", "dinsdag", "woensdag", "donderdag", 
                                      "vrijdag", "zaterdag", "zondag"))
  
  stallingsduur_frame_weken <-dataNaam[,c("Stalling", "Stallingsduur (min)", "Weekdag", "Check-out datum")]
  
  #Hernoemen kolom voor gemak
  colnames(stallingsduur_frame_weken)[colnames(stallingsduur_frame_weken)=="Stalling"] <- "Locatie"
  colnames(stallingsduur_frame_weken)[colnames(stallingsduur_frame_weken)=="Stallingsduur (min)"] <- "StallingsduurMin"
  stallingsduur_frame_weken$Locatie <- locatieNaam
  
  stallingsduur_frame_weken$stallingsduurUur <- stallingsduur_frame_weken$StallingsduurMin/60
  #stallingsduur_frame_weken <- na.omit(stallingsduur_frame_weken)
  
  stallingsduur_frame_weken$duratie <- "stub"
  
  # Classificatie toevoegen voor hoe lang een fiets staat gestald
  stallingsduur_frame_weken$duratie[(stallingsduur_frame_weken$stallingsduurUur < 1)]  <- "korter dan 1 uur"
  stallingsduur_frame_weken$duratie[(stallingsduur_frame_weken$stallingsduurUur >= 1 & stallingsduur_frame_weken$stallingsduurUur < 4)]  <- "tussen 1 en 4 uur"
  stallingsduur_frame_weken$duratie[(stallingsduur_frame_weken$stallingsduurUur >= 4 & stallingsduur_frame_weken$stallingsduurUur < 12)]  <- "tussen 4 en 12 uur"
  stallingsduur_frame_weken$duratie[(stallingsduur_frame_weken$stallingsduurUur >= 12 & stallingsduur_frame_weken$stallingsduurUur < 24)]  <- "tussen 12 en 24 uur"
  stallingsduur_frame_weken$duratie[(stallingsduur_frame_weken$stallingsduurUur >= 24)]  <- "langer dan 24 uur"
  
  stallingsduur_frame_weken$duratie <- ordered(stallingsduur_frame_weken$duratie, levels = c("korter dan 1 uur","tussen 1 en 4 uur","tussen 4 en 12 uur","tussen 12 en 24 uur","langer dan 24 uur"))
  
  return(stallingsduur_frame_weken)
}

stallingsduur_frame_weken <- verwerkStallingsduur(dataVoorbeeld, "Stadhuis")




##########
## DE VRAGEN 
## Vraag 2.a) Hoe lang staan fietsen gestald per stalling? 
# Gemiddelde stallingsduur (uren) voor Stadhuis in juni 2018 [LATEN ZIEN]
mean(stallingsduur_frame_weken$stallingsduurUur, na.rm=TRUE)
#Totaal aantal stallingen [LATEN ZIEN]
length(stallingsduur_frame_weken$stallingsduurUur)

#########
## b) Hoeveel % van de fietsen staat in juni 2018 minder dan 1 uur in stalling Stadhuis gestald?

#aantal fietsen die korter dan uur zijn gestald
length(stallingsduur_frame_weken$stallingsduurUur[(stallingsduur_frame_weken$duratie == "korter dan 1 uur")])

#Maak een frequentietabel van het dataframe
temptabel <- table(stallingsduur_frame_weken$duratie, stallingsduur_frame_weken$Weekdag)

#Aantal fietsen verdeeld over duratie ### Overzicht van verdeling stallingstijden bij Stadhuis in juni 2018
round(margin.table(temptabel, 1)/length(stallingsduur_frame_weken$stallingsduurUur)*100)
#Aantal fietsen verdeeld over de weekdagen [LATEN ZIEN]
round(margin.table(temptabel, 2)/length(stallingsduur_frame_weken$stallingsduurUur)*100)
#Hetzelfde kan bereikt worden met een iets omslachtiger alternatief
Gemiddelde_stallingsduur_per_weekdag <- aggregate(stallingsduurUur ~ Weekdag, stallingsduur_frame_weken, length)
Gemiddelde_stallingsduur_per_weekdag$stallingsduurUur <- Gemiddelde_stallingsduur_per_weekdag$stallingsduurUur/length(stallingsduur_frame_weken$stallingsduurUur)*100
Gemiddelde_stallingsduur_per_weekdag

#Verdeling duratie gestalde fietsen per dag [LATEN ZIEN] ##Vraag 2g VERSCHILLEN TUSSEN DAGEN VAN DE WEEK 
#Houd er rekening mee dat de stallingsdag kan verschillen van de vertrekdag. Alleen vertrekdag staat als naam gemeld.
dat <- round(prop.table(temptabel, 2)*100,1)
dat

#Als laatste nog wel interessant dat met de meer omslachtige aggregate we ook kunnen laten zien waar het zwaarte punt van de verdeling zit
# We geven de verdeling als de som van de uren wordt meegenomen. Dit kun je zien als het gewicht van de categorie voor het totaal aantal stallingsuren
# Als je hier ziet bijvoorbeeld dat 99% van de stallingsuren komt uit de langer dan 24 categorie kan hierop beleid gemaakt worden
Stallingsduur_per_categorie <- aggregate(stallingsduurUur ~  Weekdag + duratie, stallingsduur_frame_weken, sum)
Stallingsduur_per_categorie <- transform(Stallingsduur_per_categorie, stallingsduur_pct = ave(stallingsduurUur, Weekdag, FUN = function(x) round(x/sum(x), 3)*100))
hardTabel <- xtabs(stallingsduur_pct ~ duratie + Weekdag, Stallingsduur_per_categorie)


# Plotten in simpele R
barplot(dat, main="Verdeling Stallingsduur Fietsen per dag van de Week",
        xlab="Dag van de week",
        legend = rownames(dat)) 

# Plotten met Plotly

# Dagen van de week vastleggen
xform <- list(categoryorder = "array",
              categoryarray = c("maandag", "dinsdag", "woensdag", "donderdag", 
                                "vrijdag", "zaterdag", "zondag"))

# Data omvormen voor plotly NB! Hier kun je varieren voor temptabel voor aantalen en "dat" voor de percentage (cummulatief naar 100 per weekdag).
dat2 <- data.frame(rbind(t(temptabel)))
colnames(dat2) <- c("korter dan 1 uur","tussen 1 en 4 uur","tussen 4 en 12 uur","tussen 12 en 24 uur","langer dan 24 uur")

# Plotten als barchart
p <- plot_ly(dat2, x = xform$categoryarray, y = ~`korter dan 1 uur`, type = 'bar', name = 'korter dan een uur') %>%
  add_trace(y = ~`tussen 1 en 4 uur`, name = 'tussen 1 en 4 uur') %>%
  add_trace(y = ~`tussen 4 en 12 uur`, name = 'tussen 4 en 12 uur') %>%
  add_trace(y = ~`tussen 12 en 24 uur`, name = 'tussen tussen 12 en 24 uur') %>%
  add_trace(y = ~`langer dan 24 uur`, name = 'langer dan 24 uur') %>%
  layout(xaxis = xform, yaxis = list(title = 'Aantallen'), barmode = 'stack',legend = list(x = 0.1, y = 0.9))
p


