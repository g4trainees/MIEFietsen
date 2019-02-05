#################################################################################################
## Script data uniformisering en verwerking voor fietsenstallingsgebruik dashboard
#
# Poolopdracht G4 trainees informatiemanagement sep 2018- feb 2019
# Scrumteam Mobile Informatie Eenheid
#
# Auteurs: Jurriaan Duyne (jurriaan.duyne@utrecht.nl), Alma Raskadic en Pieter Goethart, Nizar el Haik en Dominique de Graaf.

## Omgeving selecteren (voor iedereen anders!)
#setwd("~/G4 Traineeship/Poolopdrachten/3 - Strategische opgaven (Utrecht)/Mobiele Informatie Eenheid/Dashboard")

## Packages laden (moeten eventueel nog geinstalleerd worden install.packages('PACKAGE_NAME'))
library(readr)
library(dplyr)
library(lubridate)


##### Inlezen en verwerken databestanden 

## Afspraak, zet je working directory op de map waar de app.R file in staat
# setwd("~/G4 Traineeship/Poolopdrachten/3 - Strategische opgaven (Utrecht)/Mobiele Informatie Eenheid/Dashboards/Dashboard R")

## Aanname: binnen deze map staat de submap "Data" die daar weer in heeft "Bronnen" en "Output". 
# In "Bronnen" plaatsen we de ruwe data van Lumiguide en FMS in mappen met dezelfde naam
# Binnen de FMS en Lumiguide mappen is de data dusdanig opgesplitst dat de data van elke locatie in een eigen map staat met de naam van de locatie
# Met deze mappen structuur kunnen we relatief eenvoudig over de locatie itereren en de namen toewijzen zonder al te extreme toeren uit te halen om 
# uit de String van een naam een locatie te hoeven zoeken

dataDirectory <- paste0(getwd(),"/Data")
inputDirectory <- paste0(getwd(),"/Data/Bronnen")
outputDirectory <- paste0(getwd(),"/Data/Output")

### Ondersteunende functies inlezen

## Het verwerken van de bezettingsgraad uit Lumiguide ruwe data
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

## Het verwerken van stallingsduur uit FMS ruwe data 
verwerkStallingsduur <- function(dataNaam, locatieNaam){
  
  ## Data voorbewerken
  dataNaam$Weekdag <- weekdays(dataNaam$`Check-out datum`)
  dataNaam$Weekdag <-ordered(dataNaam$Weekdag, levels=c("maandag", "dinsdag", "woensdag", "donderdag", 
                                                        "vrijdag", "zaterdag", "zondag"))
  
  stallingsduur_frame_weken <-dataNaam[,c("Stalling", "Stallingsduur (min)", "Weekdag", "Check-out datum")]
  
  #Hernoemen kolom voor gemak
  colnames(stallingsduur_frame_weken)[colnames(stallingsduur_frame_weken)=="Stalling"] <- "Locatie"
  colnames(stallingsduur_frame_weken)[colnames(stallingsduur_frame_weken)=="Check-out datum"] <- "Tijd"
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

## Aantal in en uit checken per uur verwerken uit FMS ruwe (uur)data
verwerkInOut <- function(inOutData, locatie) {
  # Eerst nog wat voorverwerken op de ruwe data
  # We voegen volledige datetimes toe
  inOutData$Checkin_datetime <- as.POSIXct(paste(inOutData$`Check-in datum`, inOutData$`Check-in tijd`))
  inOutData$Checkout_datetime <- as.POSIXct(paste(inOutData$`Check-out datum`, inOutData$`Check-out tijd`))
  
  # En ronden deze af
  inOutData$Checkin_datetime_floor <- floor_date(inOutData$Checkin_datetime, "hour")
  inOutData$Checkout_datetime_floor <- floor_date(inOutData$Checkout_datetime, "hour")
  
  # Dummy variabele en aggregatie
  inOutData$aantal_stallingen <- 1
  checkins <- aggregate(x = inOutData$aantal_stallingen, by = list(inOutData$Checkin_datetime_floor), FUN = "sum")
  colnames(checkins) <- c("Tijd","Aantal checkins")
  checkouts <- aggregate(x = inOutData$aantal_stallingen, by = list(inOutData$Checkout_datetime_floor), FUN = "sum")
  colnames(checkouts) <- c("Tijd","Aantal checkouts")
  
  # Doorlopende lijst van begin tot eind tijd van bestand maken
  startDatumTijdlocal <- floor_date(inOutData$Checkout_datetime[1], "hour")
  eindDatumTijdlocal <- floor_date(inOutData$Checkout_datetime[length(inOutData$Checkout_datetime)], "hour")
  
  uurSequentieLocal <- seq(startDatumTijdlocal, eindDatumTijdlocal, by="hour")
  checkInOutPerUurTempLocal <- data.frame(uurSequentieLocal,rep(0,length(uurSequentieLocal)),rep(0,length(uurSequentieLocal)),rep('locatie',length(uurSequentieLocal)))
  colnames(checkInOutPerUurTempLocal) <- c("Tijd","Aantal checkins", "Aantal checkouts", "Locatie")
  
  
  # En dan mergen we het dataframe met de bezettingslijsten
  samenVoeging <- merge(checkInOutPerUurTempLocal,checkins, by = "Tijd", all.x = TRUE)
  samenVoeging <- merge(samenVoeging,checkouts, by = "Tijd", all.x = TRUE)
  samenVoeging['Locatie'] <- locatie
  
  eindResultaat <- samenVoeging[c("Tijd", "Aantal checkins.y", "Aantal checkouts.y","Locatie")]
  eindResultaat[is.na(eindResultaat)] <- 0
  colnames(eindResultaat) <- c("Tijd","Aantal checkins", "Aantal checkouts","Locatie")
  
  return(eindResultaat)
}

##### Verwerken van de data

## FMS data 

# Data frame stub voor duratie
stallingsduur <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(stallingsduur) <- c("Locatie","StallingsduurMin","Weekdag","Check-out datum","stallingsduurUur","duratie")

# Data frame stub voor in en uit checken
checkInOutPerUur <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(checkInOutPerUur) <- c("Tijd","Aantal checkins", "Aantal checkouts", "Locatie")

# input directory
inputDirectoryFMS <- paste0(getwd(),"/Data/Bronnen/FMS")

# Nu itereren over locatie en bestand binnen de locatie
for (naam in list.files(path = inputDirectoryFMS)){
  
  currentFileDirectory <- paste0(getwd(),"/Data/Bronnen/FMS/",naam)
  print(currentFileDirectory)
  
  for (filenaam in list.files(path = currentFileDirectory)){
    
    currentFile <- paste0(currentFileDirectory,"/",filenaam)
    
    currentData <- read_delim(currentFile,
                              ";", escape_double = FALSE, col_types = cols(`Check-in datum` = col_date(format = "%d-%m-%y"),
                                                                           `Check-in tijd` = col_time(format = "%H:%M:%S"),
                                                                           `Check-out datum` = col_date(format = "%d-%m-%y"),
                                                                           `Check-out tijd` = col_time(format = "%H:%M:%S")),
                              trim_ws = TRUE)
    
    checkInOutPerUur <- rbind(checkInOutPerUur,verwerkInOut(currentData, naam))
    
    stallingsduur <- rbind(stallingsduur,verwerkStallingsduur(currentData, naam))
    
    print(currentFile)
  }
}

## Lumiguide
inputDirectoryLumiguide <- paste0(getwd(),"/Data/Bronnen/Lumiguide")

# Stub voor bezettingsgraad
bezetting <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(bezetting) <- c("Tijd", "BezettingspercentageUur", "VrijpercentageUur", "Locatie")

# Bezettingsgraad
for (naam in list.files(path = inputDirectoryLumiguide)){
  
  currentFileDirectory <- paste0(inputDirectoryLumiguide,"/",naam)
  print(currentFileDirectory)
  
  for (filenaam in list.files(path = currentFileDirectory)){
    
    currentFile <- paste0(currentFileDirectory,"/",filenaam)
    
    currentData <- read_csv(currentFile, 
                            col_types = cols(`time (UTC)` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))
    
    bezetting <- rbind(bezetting,verwerkBezetting(currentData,naam))
    
    print(currentFile)
  }
}

### Coordinaten om op kaart te plotten
coordinaten <- read_delim("Data/coordinaten.csv", 
                          ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)


### Output opslaan in output directory
saveRDS(bezetting, file = paste0(outputDirectory,"/", "bezetting.RDS"))
saveRDS(stallingsduur, file = paste0(outputDirectory,"/", "stallingsduur.RDS"))
saveRDS(checkInOutPerUur, file = paste0(outputDirectory,"/", "checkInOutPerUur.RDS"))
saveRDS(coordinaten, file = paste0(outputDirectory,"/", "coordinaten.RDS"))






