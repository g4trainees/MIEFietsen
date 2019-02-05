#################################################################################################
## Script data verwerking Vraag 3
#
# Poolopdracht G4 trainees informatiemanagement sep 2018- feb 2019
# Scrumteam Mobile Informatie Eenheid
#
# Auteur: Jurriaan Duyne (jurriaan.duyne@utrecht.nl)

## Omgeving selecteren (voor iedereen anders!) directory waar file "FMS_2018_06_Utrecht_Stadhuis_ruwedata_transacties.csv" in staat
#setwd("~/G4 Traineeship/Poolopdrachten/3 - Strategische opgaven (Utrecht)/Mobiele Informatie Eenheid/Data")

## Packages laden (moeten eventueel nog geinstalleerd worden install.packages('PACKAGE_NAME'))
library(readr)
library(dplyr)
library(lubridate)
library(plotly)

# probeer plotly en als je het kunt installeren draai library(plotly)


## Inlezen databestanden 
# We hebben gekozen om van verschillende bronnen fietsdata van het stadhuis te selecteren zodat we deze ook kunnen vergelijken
# FMS data alle transacties
FMS_data_2018_alle_stallingen_transacties <- read_delim("FMS_data_2018_alle_stallingen_transacties.csv", 
                                                        ";", col_types = cols(Datum = col_date(format = "%d-%m-%y")),
                                                        escape_double = FALSE, trim_ws = TRUE)

# Stadhuis selecteren
FMS_stadhuis_dagniveau <- filter(FMS_data_2018_alle_stallingen_transacties, Stalling == 'Utrecht Stadhuis')
# Juni selecteren
FMS_stadhuis_dagniveau_juni <- filter(FMS_stadhuis_dagniveau, Maand == 'juni')

View(FMS_stadhuis_dagniveau_juni)

# FMS ruwe data transacties
FMS_2018_06_Stadhuis_ruwedata_transacties <- read_delim("FMS_2018_06_Utrecht_Stadhuis_ruwedata_transacties.csv", 
                                  ";", escape_double = FALSE, col_types = cols(`Check-in datum` = col_date(format = "%d-%m-%y"), 
                                                  `Check-in tijd` = col_time(format = "%H:%M:%S"), 
                                                  `Check-out datum` = col_date(format = "%d-%m-%y"), 
                                                 `Check-out tijd` = col_time(format = "%H:%M:%S")), 
                                                                    trim_ws = TRUE)

View(FMS_2018_06_Stadhuis_ruwedata_transacties)


####################################################################################
## Verwerken om vragen te bewantwoorden

## Vraag 3:
# Aantal in en uit checken per uur?

# Eerst maken we een uursequentie voor juni
juni_per_uur <- seq(as.POSIXct("2018-06-01 00:00:00"), as.POSIXct("2018-06-30 23:00:00"), by="hour")

# Hiervan maken we een dataframe
stadhuis_juni_check_in_out_per_uur_temp <- data.frame(juni_per_uur,rep(0,length(juni_per_uur)),rep(0,length(juni_per_uur)))
colnames(stadhuis_juni_check_in_out_per_uur_temp) <- c("Tijd","Aantal checkins", "Aantal checkouts")

# Eerst nog wat voorverwerken op de ruwe data
# We voegen volledige datetimes toe
FMS_2018_06_Stadhuis_ruwedata_transacties$Checkin_datetime <- as.POSIXct(paste(FMS_2018_06_Stadhuis_ruwedata_transacties$`Check-in datum`, FMS_2018_06_Stadhuis_ruwedata_transacties$`Check-in tijd`))
FMS_2018_06_Stadhuis_ruwedata_transacties$Checkout_datetime <- as.POSIXct(paste(FMS_2018_06_Stadhuis_ruwedata_transacties$`Check-out datum`, FMS_2018_06_Stadhuis_ruwedata_transacties$`Check-out tijd`))

# En ronden deze af
FMS_2018_06_Stadhuis_ruwedata_transacties$Checkin_datetime_floor <- floor_date(FMS_2018_06_Stadhuis_ruwedata_transacties$Checkin_datetime, "hour")
FMS_2018_06_Stadhuis_ruwedata_transacties$Checkout_datetime_floor <- floor_date(FMS_2018_06_Stadhuis_ruwedata_transacties$Checkout_datetime, "hour")

# Dummy variabele en aggregatie
FMS_2018_06_Stadhuis_ruwedata_transacties$aantal_stallingen <- 1
checkins <- aggregate(x = FMS_2018_06_Stadhuis_ruwedata_transacties$aantal_stallingen, by = list(FMS_2018_06_Stadhuis_ruwedata_transacties$Checkin_datetime_floor), FUN = "sum")
colnames(checkins) <- c("Tijd","Aantal checkins")
checkouts <- aggregate(x = FMS_2018_06_Stadhuis_ruwedata_transacties$aantal_stallingen, by = list(FMS_2018_06_Stadhuis_ruwedata_transacties$Checkout_datetime_floor), FUN = "sum")
colnames(checkouts) <- c("Tijd","Aantal checkouts")

# En dan mergen we het dataframe met de bezettingslijsten
stadhuis_juni_check_in_out_per_uur_temp <- merge(stadhuis_juni_check_in_out_per_uur_temp,checkins, by = "Tijd", all.x = TRUE)
stadhuis_juni_check_in_out_per_uur_temp <- merge(stadhuis_juni_check_in_out_per_uur_temp,checkouts, by = "Tijd", all.x = TRUE)

stadhuis_juni_check_in_out_per_uur <- stadhuis_juni_check_in_out_per_uur_temp[c("Tijd", "Aantal checkins.y", "Aantal checkouts.y")]
stadhuis_juni_check_in_out_per_uur[is.na(stadhuis_juni_check_in_out_per_uur)] <- 0
colnames(stadhuis_juni_check_in_out_per_uur) <- c("Tijd","Aantal checkins", "Aantal checkouts")

write_csv(stadhuis_juni_check_in_out_per_uur, "stadhuis_juni_check_in_out_per_uur.csv")

# workspace opschonen
rm(list=setdiff(ls(), c("stadhuis_juni_check_in_out_per_uur", "Lumiguide_2018_06_Stadhuis_uurdata","FMS_data_2018_alle_stallingen_transacties")))

# Plot de data plotly benodigd
data_plot <- plot_ly(stadhuis_juni_check_in_out_per_uur, x = ~Tijd, y = ~`Aantal checkins`, name = 'Aantal Checkins', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
  add_trace(y = ~`Aantal checkouts`, name = 'Aantal checkouts', line = list(color = 'rgb(22, 96, 167)', width = 2)) %>%
  layout(title = "Aantal fietsen checkins en checkouts bij het stadhuis in juni 2018",
         xaxis = list(title = "Uren"),
         yaxis = list (title = "Aantallen"))

data_plot
