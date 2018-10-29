# Fitness and Freshness Graph
# Analyse and plot graph of fitness and freshness using Strava running and cycling data
# bnwicks@gmail.com

library(tidyverse)
library(gridExtra)

# Assumptions
impulse_runningPerKm <- 13 # between ~10 - 15
impulse_ridingPerKm <-  3.3 # between ~3 - 5

# fitness = 1*impulse, -20% per week, ~2% per day, use ~3 months data
fitness_constant <- 0.98
impulse_to_fitness <- 1/45

# fatigue = 5*impulse, -80% per week, ~15% per day, use ~3 months data
fatigue_constant <- 0.85
impulse_to_fatigue <- 0.125

# form = difference between fitness and fatigue

# calculate form, fitness and fatigue for each day
df_raw <- read.csv(file = "./data/activities.csv") # read data

#Specify Start Date
start_date <- min(as.numeric(as.Date(df_raw$date)))
#start_date <- as.numeric(as.Date("2018-08-21"))
end_date <- max(as.numeric(as.Date(df_raw$date)))

df_raw <- df_raw %>%
  filter(as.numeric(as.Date(date)) >= start_date)

df <- df_raw %>%
  mutate(impulse = ifelse(type=="Ride", impulse_ridingPerKm * distance / 1000, impulse_runningPerKm * distance / 1000))

df %>% ggplot(aes(x = as.Date(date), y = impulse, color = type)) +
  geom_point()

f_fitness <- function(i_date) {
  df_temp <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(as.numeric(date) - as.numeric(as.Date(i_date, origin = "1970-01-01")) <= 0) %>%
    mutate(relative_impulse = 0) %>%
    mutate(relative_impulse = impulse * impulse_to_fitness * fitness_constant^(as.numeric(as.Date(i_date, origin = "1970-01-01") - as.numeric(date))))
  return(sum(df_temp$relative_impulse))
  #return(df_temp)
}

f_fatigue <- function(i_date) {
  df_temp <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(as.numeric(date) - as.numeric(as.Date(i_date, origin = "1970-01-01")) <= 0) %>%
    mutate(relative_impulse = 0) %>%
    mutate(relative_impulse = impulse * impulse_to_fatigue * fatigue_constant^(as.numeric(as.Date(i_date, origin = "1970-01-01") - as.numeric(date))))
  return(sum(df_temp$relative_impulse))
  #return(df_temp)
}

df_fff <- data_frame("date" = start_date:end_date)

df_fff <- df_fff %>%
  mutate(fitness = sapply(date, f_fitness)) %>%
  mutate(fatigue = sapply(date, f_fatigue)) %>%
  mutate(form = fitness - fatigue)

df_tidy <- df_fff %>% gather(type = fitness:form)

plot_1 <- df_tidy %>% ggplot(aes(x = as.Date(date, origin = "1970-01-01"), y = value, col = key)) +
  geom_line(size=1)

plot_2 <- df %>% ggplot(aes(x = as.Date(date, origin = "1970-01-01"), y = impulse, fill=type)) +
  geom_bar(stat = "identity")

grid.arrange(plot_1, plot_2, ncol = 1)
