load("Airplane_crashes.Rdata")
library(ggplot2)

# Death vs Phase plot

g <- ggplot(final_df, aes(Phase, `Total deaths`, fill = Phase))
g + geom_bar(stat="identity", width = 0.5) +
  labs(title="Phase of accident vs Total Deaths") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme_minimal() +
  labs(x = "Phase of Accident") +
  scale_fill_discrete(name = "Accident Type", labels = c("Approach", "En Route", "Initial Climb", "Landing", "Maneuvering", "Standing", "Take off", "Taxi", "Unknown"))
