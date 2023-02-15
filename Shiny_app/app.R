library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library("countrycode")
library("rworldmap")
library(DT)
library(plotly)

data <- load("Airplane_crashes.Rdata")

data_year <- final_df %>% group_by(Date) %>% summarise(sum(`Total deaths`), 
                                                       sum(`Crew deaths`),
                                                       sum(`Passenger deaths`),
                                                       sum(`Ground deaths`))
colnames(data_year) <- c("Date", "Total deaths","Crew deaths", "Passenger deaths", "Ground deaths")
data_year <- as.data.frame(data_year)

#------------------------------------------------

temp <- final_df
vec <- numeric(length = 544)
vec <- sapply(vec, function(x) 1)
temp$freq <- c(vec)

data_company <- temp %>% group_by(Aircraft) %>% summarise(sum(`Total deaths`), 
                                                          sum(`Crew deaths`),
                                                          sum(`Passenger deaths`),
                                                          sum(`Ground deaths`),
                                                          sum(`freq`))
colnames(data_company) <- c("Aircraft", "Total deaths","Crew deaths", "Passenger deaths", "Ground deaths", "freq")
for(i in 2:5)
{
  data_company[ ,i] = data_company[ ,i]/data_company$freq
}
data_company <- as.data.frame(data_company)

#------------------------------------------------
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

#--------------------------------------------------------------------------------------------------

ui = dashboardPage(skin = "red",
  dashboardHeader(title = "Aircraft Accidents"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deaths vs Time", tabName = "deaths_vs_time", icon = icon("clock")),
      menuItem("Deaths vs Type", tabName = "deaths_vs_type", icon = icon("plane")),
      menuItem("Deaths vs Company", tabName = "deaths_vs_company", icon = icon("plane")),
      menuItem("Comapnies over Time", tabName = "company_over_time", icon = icon("plane")),
      menuItem("Deaths vs Phase", tabName = "deaths_vs_phase", icon = icon("road")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Summary", tabName = "summary", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("deaths_vs_time",
              box(plotlyOutput("death_vs_time")),
              box(selectInput("death_time_c",
                              label = "Deaths vs Time",
                              choices = list("Total deaths", "Crew deaths", "Passenger deaths"),
                              selected = "Total deaths"),
                  sliderInput("death_time",
                              label = "Deaths vs Year",
                              min = min(final_df$Date),
                              max = max(final_df$Date),
                              round = TRUE,
                              value = c(min(final_df$Date), max(final_df$Date))), width = 3)),
      tabItem("deaths_vs_type",
              box(plotOutput("death_vs_type")),
              box(selectInput("Aircraft_Type",
                              label = "Deaths vs Aircraft Type",
                              choices = list("Total deaths", "Crew deaths", "Passenger deaths"),
                              selected = "Total deaths"), width = 4)),
      tabItem("deaths_vs_company",
              box(plotOutput("death_vs_company"), width = 12),
              box(radioButtons("phase_type_c", h3("Type"),
                               choices = list("COM", "INH",
                                              "INB", "EXG",
                                              "MIL", "EXS")), width = 4)),
      tabItem("company_over_time",
              box(plotOutput("death_time_company"), width = 8),
              box(radioButtons("death_time_com", 
                                     h3("Aircraft Model"), 
                                     choices = list("Airbus", "Antonov",
                                                    "Boeing", "Douglas",
                                                    "Ilyushin", "Lockheed",
                                                    "McDonnell Douglas", "Tupolev")), width = 4)),
      tabItem("deaths_vs_phase",
              box(plotOutput("death_vs_phase"), width = 8),
              box(selectInput("death_phase",
                              label = "Deaths vs Phase",
                              choices = list("Total deaths", "Crew deaths", "Passenger deaths"),
                              selected = "Total deaths"), width = 4)),
      tabItem("map",
              fluidPage(h6 = "Deaths vs Countries", plotOutput("mplot"))),
      tabItem("summary",
              fluidPage(h1 = "Aircraft Accidents",
              dataTableOutput("Final_df")))
    ),
  ),
  title = "Dashboard example"
)

server <- function(input, output) {
  
  output$death_vs_time <- renderPlotly({
    p <- ggplot(data_year, aes(x = Date, y = data_year[ , input$death_time_c])) +
      coord_cartesian(xlim = input$death_time) +
      geom_point(col = "red", aes(text = paste("Year", Date,
                                               "<br>Deaths", data_year[ , input$death_time_c]))) +
      geom_line() +
      geom_smooth(method = "lm") +
      labs(x = "Year", y = input$death_time_c) +
      theme_light()
    
    ggplotly(p, tooltip = 'text')
  })
  
  output$death_vs_type <- renderPlot({
    ggplot(final_df, aes(x =Type, y = final_df[ ,input$Aircraft_Type], fill = Type)) + 
      geom_bar(stat = "identity", width = 0.5) +
      labs(x = "Aircraft Type", y = input$Aircraft_Type) +
      theme_minimal() +
      scale_fill_discrete( labels = c("COM- Commercial","EXG-Attacked using weapons",
                                      "EXS-Attacked by aircrafts",
                                      "INB-Bombing",
                                      "INH- Hijacking",
                                      "MIl-	Military accident"))
  }, height = 400, width = 600)
  
  output$death_vs_company <- renderPlot({
    data <- load("Airplane_crashes.Rdata")
    type_1 <- final_df$Aircraft[which(final_df$Type == input$phase_type_c)]
    df_types <- data.frame(type_1, final_df$`Total deaths`[which(final_df$Type == input$phase_type_c)])
    colnames(df_types) <- c("Aircraft","Total")
    
    ggplot(df_types, aes(x = Aircraft, y = Total, fill = Aircraft)) + 
      geom_bar(stat = "identity", width = 0.5) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme_minimal() +
      labs(x = "Aircraft Company", y = "Total deaths") +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  output$death_vs_companyr <- renderPlot({
    ggplot(data_company, aes(x = Aircraft, y = data_company[ ,input$Aircraft_Company_ratio], fill = Aircraft)) + 
      geom_bar(stat = "identity", width = 0.5) +
      labs(x = "Aircraft Company", y = input$Aircraft_Company_ratio) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
  }, height = 400, width = 1200)
  
  output$death_time_company <- renderPlot({ 
    type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == input$death_time_com)]
    type_2 <- final_df$Aircraft[which(final_df$Aircraft == input$death_time_com)]
    df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == input$death_time_com)])
    
    ggplot(df_types, aes(df_types[,3], df_types[,1]))+
      geom_point()+
      geom_smooth(method = 'lm',
                  formula = y~x,
                  se = F)+
      ylim(0,500) +
      theme_minimal() +
      labs(x = "Date", y = "Total deaths")
  })
  
  output$death_vs_phase <- renderPlot({
    ggplot(final_df, aes(x = Phase, y = final_df[ ,input$death_phase], fill = Phase)) + 
      geom_bar(stat = "identity", width = 0.5) +
      labs(x = "Phase", y = input$death_phase) +
      theme_minimal() +
      scale_fill_discrete( labels = c("APR - Approach", "ENR - En Route", "ICL - Initial Climb", "LDG - Landing", "MNV - Maneuvering", "STD - Standing", "TOF - Take off", "TXI - Taxi", "UNK - Unknown"))
  })
  
  output$mplot <- renderPlot({
    mapCountryData( sPDF , nameColumnToPlot="Deaths"
                    , catMethod= "categorical"
                    , mapTitle= "Deaths vs Country"
                    , colourPalette= "palette"
                    , oceanCol= "lightblue"
                    , missingCountryCol = "white")
  }, height = 800, width = 1200)
  
  output$Final_df <- renderDataTable(final_df)
}

shinyApp(ui = ui, server = server)
