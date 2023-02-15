library("tidyverse") # load dplyr, ggplot2, stringr, etc.
library("countrycode") # get ISO code from country names
library("rworldmap")

data <- load("Airplane_crashes.Rdata")

data_with_iso <- final_df %>%
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Location, 
    origin = "country.name", 
    destination = "iso3c")
  )

data_with_iso

data_country <- data_with_iso %>% group_by(Iso3) %>% summarise(sum(`Total deaths`), 
                                                       sum(`Crew deaths`),
                                                       sum(`Passenger deaths`),
                                                       sum(`Ground deaths`))
colnames(data_country) <- c("Iso3", "Total deaths","Crew deaths", "Passenger deaths", "Ground deaths")


sPDF <- joinCountryData2Map( data_country,
                             joinCode = "ISO3",
                             nameJoinColumn = "Iso3" )

#creating a user defined colour palette
op <- palette(c("lightgreen","yellow","orange","red"))
#find quartile breaks
cutVector <- c(50, 400, 800, 1800, 9000)
#quantile(sPDF@data[["Total deaths"]],na.rm=TRUE)
#c(50, 200, 800, 1800, 9000)
#classify the data to a factor
sPDF@data[["Deaths"]] <- cut(sPDF@data[["Total deaths"]]
                                        , cutVector
                                        , include.lowest=TRUE )
levels(sPDF@data[["Deaths"]]) <- c("low", "med", "high", "vhigh")
#mapping
mapCountryData( sPDF , nameColumnToPlot="Deaths"
                    , catMethod= "categorical"
                    , mapTitle= "Deaths vs Country"
                    , colourPalette= "palette"
                    , oceanCol= "lightblue"
                    , missingCountryCol = "white")

