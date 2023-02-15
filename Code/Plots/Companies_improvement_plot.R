load("Airplane_crashes.Rdata")
library(ggplot2)

# improvement of companies plot
# Airbus---------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Airbus")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Airbus")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Airbus")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Airbus") +
  theme_minimal() 

# Antonov-------------------------------------------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Antonov")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Antonov")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Antonov")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Antonov") +
  theme_minimal() 

# Boeing------------------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Boeing")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Boeing")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Boeing")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Boeing") +
  theme_minimal() 
# Douglas--------------------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Douglas")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Douglas")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Douglas")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Douglas") +
  theme_minimal() 

# Ilyushin-------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Ilyushin")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Ilyushin")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Ilyushin")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Ilyushin") +
  theme_minimal() 

# Lockheed-------------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Lockheed")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Lockheed")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Lockheed")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Lockheed") +
  theme_minimal()

# McDonnell Douglas-----------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "McDonnell Douglas")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "McDonnell Douglas")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "McDonnell Douglas")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "McDonnell Douglas") +
  theme_minimal() 

# Tupolev------------------------------

data <- load("Airplane_crashes.Rdata")
type_1 <- final_df$`Total deaths`[which(final_df$Aircraft == "Tupolev")]
type_2 <- final_df$Aircraft[which(final_df$Aircraft == "Tupolev")]
df_types <- data.frame(type_1, type_2, final_df$Date[which(final_df$Aircraft == "Tupolev")])

ggplot(df_types, aes(df_types[,3], df_types[,1]))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,
              se = F)+
  labs(x = "Year", y = "Number of Deaths", title = "Tupolev") +
  theme_minimal()
