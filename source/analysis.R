incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(ggplot2)
library(reshape2)
options(scipen = 999)

raceRatio <- incarcerations %>%
  select(white_pop_15to64, black_pop_15to64) %>%
  mutate(white = sum(white_pop_15to64, na.rm = TRUE), black = sum(black_pop_15to64, na.rm = TRUE)) %>%
  distinct(white, .keep_all = TRUE) %>%
  mutate(ratio = white/black) %>%
  pull(ratio)

byState <- incarcerations %>%
  group_by(state) %>%
  summarise("total" = sum(total_pop)) %>%
  filter(total ==max(total)) %>%
  pull(state)

meanSize <- incarcerations %>%
  group_by(county_name) %>%
  summarise(mean_size = mean(land_area)) %>%
  summarise(mean_all = mean(mean_size, na.rm = TRUE)) %>%
  pull(mean_all)

pop1970 <- incarcerations %>%
  filter(year == "1970") %>%
  filter(total_pop == max(total_pop)) %>%
  pull(total_pop)

pop2018 <- incarcerations %>%
  filter(year == "2018") %>%
  filter(total_pop == max(total_pop)) %>%
  pull(total_pop)
popDiff <- pop2018 - pop1970

mostIncarcerations <- incarcerations %>%
  group_by(county_name) %>%
  summarise(total = sum(total_pop)) %>%
  filter(total == max(total)) %>%
  pull(total)

LAcounty <- incarcerations %>%
  filter(state == "CA") %>%
  filter(fips < "6050") %>%
  filter(fips > "6033")

LAoverTime <- ggplot(LAcounty, aes(x=year, y=total_pop, colour = county_name)) + geom_line() + labs(y = "population", title = "LA vs Nearby County population Over Time")


NYcounty <- incarcerations %>%
  filter(state == "NY") %>%
  filter(county_name == "New York County") %>%
  filter(year > "1990") %>%
  select(black_pop_15to64, white_pop_15to64, latinx_pop_15to64)

NYGraph <- melt(NYcounty)

NYRace <- NYGraph %>%
  group_by(variable) %>%
  summarize(population = sum(value))

NYPlot <- ggplot(NYRace, aes(x = variable, y = population)) + geom_bar(stat = "identity") + labs(x = "Race", y = "Population", title = "Population of NY county jail from 1990 by Race")

