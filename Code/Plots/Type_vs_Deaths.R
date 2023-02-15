load("Airplane_crashes.Rdata")
library(ggplot2)

#ggplot for aircraft accident type vs total deaths
g <- ggplot(final_df, aes(Type, `Total deaths`, fill = Type))
g + geom_bar(stat="identity", width = 0.5) +
  labs(title="Aircraft accident type vs Total deaths") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme_minimal() +
  labs(x = "Accident Type") +
  scale_fill_discrete(name = "Accident Type", labels = c("COM-Commercial Accidents", "EXG-Attacked using weapons", "EXS-Attacked by aircrafts", "INB-Bombing", "INH-Hijacking", "MIL-Military accidents"))
