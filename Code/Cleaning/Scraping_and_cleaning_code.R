library(tidyverse)
library(rvest)
library(stringr)

html <- read_html("https://en.wikipedia.org/wiki/List_of_deadliest_aircraft_accidents_and_incidents")


tab <- html_table(html)

aircraft_data <- tab[[4]]

dims <- dim(aircraft_data)

temp_data <- aircraft_data[c(2:dims[1]), ]

#------------(Data cleaning)-----------------------------------

temp_data <- temp_data[-205,]
temp_data <- temp_data[-288,]
temp_data <- temp_data[-333,]
temp_data <- temp_data[-337,]
temp_data <- temp_data[-423,]
temp_data <- temp_data[-477,]
temp_data <- temp_data[-485,]
temp_data <- temp_data[-496,]
temp_data <- temp_data[-525,]
temp_data <- temp_data[-511,]

#-----------------------------------------------------------------


deaths_total <- temp_data[ , 1]
deaths_crew <- temp_data[ , 2]
deaths_passengers <- temp_data[ , 3]
deaths_ground <- temp_data[ , 4]


type <- temp_data[ , 6]

aircraft <- temp_data[ , 8]

location <- temp_data[ , 9]
location <- sapply(location, function(x) word(x, -1))

phase <- data.frame(Phase = substr(temp_data$Phase, 1, 3))

date <- temp_data[ , 13]
date <- sapply(date, function(x) as.numeric(substr(x, 1, 4)))

#-----------------(Data cleaning)------------------------------------------

deaths_total[1,1] <- "1700"
deaths_total[2,1] <- "1000"

deaths_ground[1,1] <- "1600"
deaths_ground[2,1] <- "900"
deaths_ground[21,1] <- "249"
deaths_ground[98,1] <- "17"

deaths_passengers[65,1] <- "151"
deaths_passengers[94,1] <- "136"
deaths_passengers[98,1] <- "110"

location[21] <- "DR Congo"
location[131] <- "Namibia"
location[419] <- "Papua New Guinea"
location[428] <- "Zimbabwe"
location[54] <- "Côte d'Ivoire"
location[526] <- "Côte d'Ivoire"
location[361] <- "Costa Rica"

#--------------------------------------------------------------

# Converting variables from character to numeric

deaths_total <- sapply(deaths_total, as.numeric)
deaths_crew <- sapply(deaths_crew, as.numeric)
deaths_passengers <- sapply(deaths_passengers, as.numeric)
deaths_ground <- sapply(deaths_ground, as.numeric)


final_df <- data.frame(deaths_total, deaths_crew, deaths_passengers, deaths_ground,
                       type, aircraft, location, phase, date)

#------------------------------------------------

final_df$Aircraft <- word(final_df$Aircraft, 1)
for(i in 1:544)
{
  if(final_df$Aircraft[i] == "McDonnell")
  {
    final_df$Aircraft[i] = "McDonnell Douglas"
  }
  if(final_df$Aircraft[i] == "Sud")
  {
    final_df$Aircraft[i] = "Sud Aviation"
  }
  if(final_df$Aircraft[i] == "de")
  {
    final_df$Aircraft[i] = "de Havilland"
  }
}

#------------------------------------------------

# final cleaned workable data frame

colnames(final_df) <- c("Total deaths","Crew deaths", "Passenger deaths", "Ground deaths", "Type", "Aircraft", "Location", "Phase", "Date")
View(final_df)
save(final_df, file = "Airplane_crashes.Rdata")

