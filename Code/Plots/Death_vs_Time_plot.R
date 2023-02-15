library(ggplot2)
library(tidyverse)
library(plotly)

load("Airplane_crashes.Rdata")

data <- final_df %>% group_by(Date) %>% summarise(sum(`Total deaths`), 
                                                  sum(`Crew deaths`),
                                                  sum(`Passenger deaths`),
                                                  sum(`Ground deaths`))
colnames(data) <- c("Date", "Total deaths","Crew deaths", "Passenger deaths", "Ground deaths")

deaths_time <- ggplot(data, aes(x = Date, y = `Total deaths`)) +
  geom_point(col = "red") +
  geom_line() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Deaths vs Year", x = "Year", y = "Total deaths") +
  theme_light()

ggplotly(deaths_time, tooltip = c("x", "y"))

