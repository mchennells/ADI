library(dplyr)
library(tsibble)

weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)


weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)
