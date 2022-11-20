library(tidyverse)
library(ggplot2)

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# avg_white_prison_pop
# avg_black_prison_pop
# avg_female_prison_pop
# avg_male_prison_pop
#----------------------------------------------------------------------------#

avg_white_prison_pop <- data %>%
  filter(year == 2018) %>%
  select(white_jail_pop) %>%
  summarise(avg = mean(white_jail_pop, na.rm = TRUE)) %>%
  pull(avg)

avg_black_prison_pop <- data %>%
  filter(year == 2018) %>%
  select(black_jail_pop) %>%
  summarise(avg = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(avg)

avg_female_prison_pop <- data %>%
  filter(year == 2018) %>%
  select(female_jail_pop) %>%
  summarise(avg = mean(female_jail_pop, na.rm = TRUE)) %>%
  pull(avg)

avg_male_prison_pop <- data %>%
  filter(year == 2018) %>%
  select(male_jail_pop) %>%
  summarise(avg = mean(male_jail_pop, na.rm = TRUE)) %>%
  pull(avg)

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# get_year_jail_pop gets the total jail population for each year from the dataset
# plot_jail_pop_for_us plots the returned data frame from the above function calculating the total jail population in a bar graph of year vs yearly jail population.
#----------------------------------------------------------------------------#
# This function gets the total jail population for each year from the dataset
get_year_jail_pop <- function() {
  year_jail_pop <- data %>%
    group_by(year) %>%
    summarise(year_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(year, year_jail_pop)
  return(year_jail_pop)
}

year_jail_pop <- get_year_jail_pop()

# This function plots the returned data frame from the above function calculating the total jail population in a bar graph of year vs yearly jail population.
plot_jail_pop_for_us <- function() {
  jail_pop_chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = year_jail_pop)) +
    labs(x = "Year", y = "Jail Population Per Year", title = "Yearly Growth
    of Prison Population from 1970-2018")
  return(jail_pop_chart)
}

jail_pop_chart <- plot_jail_pop_for_us()

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# get_year_jail_pop_states
# plot_jail_pop_for_states
#----------------------------------------------------------------------------#

# This function creates a dataframe from the provided states vector with the total jail population for each year from the dataset
get_year_jail_pop_states <- function(states) {
  growth_by_state <- data %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(year_jail_pop = sum(total_pop, na.rm = TRUE))
  return(growth_by_state)
}

# This function plots the returned data frame from the above function for the states calculating the total jail population in graph of year vs yearly jail population, colored by state.
plot_jail_pop_for_states <- function(states) {
  jail_pop_chart <- ggplot(get_year_jail_pop_states(states)) +
    geom_col(mapping = aes(x = year, y = year_jail_pop, color = state)) +
    labs(x = "Year", y = "Jail Population Per Year", title = "Yearly Growth
    of Prison Population", caption = "Yearly Growth of Prison Population
    from 1970-2018 in Selected States")
  return(jail_pop_chart)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#

# This function creates a data frame of the black population percentage as a variable and the black jail population percentage as a variable, as well as the total jail population.
inequality_chart <- function() {
  inequality <- data %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    mutate(
      black_pop_percent = (black_pop_15to64 / total_pop_15to64) * 100,
      black_jail_percent = (black_jail_pop / total_jail_pop) * 100
    ) %>%
    filter(black_jail_percent < 100) %>%
    select(
      state, county_name, black_pop_percent,
      black_jail_percent, total_jail_pop
    )
  return (inequality)
}

# This function takes the above chart and plots it on a graph of the black jail percentage vs the total jail population, colored by state. 
graph_inequalities <- function() {
  jail_pop_chart <- ggplot(inequality_chart) +
    geom_point(mapping = aes(
      x = black_jail_percent,
      y = total_jail_pop, color = state
    ) +
    labs(
      x = "Percent of Jail Population that are Black",
      y = "Total Jail Population",
      title = "Black Jail Population vs Total Jail Population",
      caption = "Percentage of Black Jail
      Population vs the Total Jail Population per County"
    )
  return (jail_pop_chart)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#

library("stringr")

# Function that maps the jail population vs total jail population of black americans across the United States.
plot_map <- function() {

  # get state lat long data prepared to do a join
  states <- map_data("state") %>%
    rename(state = region) %>%
    mutate(state = str_to_title(state))

  # similar to inequality chart but make it more condensed for mapping
  usmap_chart <- data %>%
    filter(year == 2018) %>%
    mutate(black_jail_percent = (black_jail_pop / total_jail_pop) * 100) %>%
    filter(black_jail_percent < 100) %>%
    select(state, black_jail_percent) %>%
    group_by(state) %>%
    mutate(state = state.name[match(state, state.abb)]) %>%
    na.omit() %>%
    summarise(average = mean(black_jail_percent))

  # join to be able to map with lat long data
  data_join <- inner_join(states, usmap_chart, by = "state")

  # toss into gg plot
  map <- ggplot() +
    geom_polygon(
      data = data_join, aes(x = long, y = lat, group = group, fill = average),
      color = "white", size = 0.2
    )

  map <- map + scale_fill_continuous(
    name = "Black Prison Population Percentage",
    low = "white", high = "red", na.value = "grey50"
  ) + labs(title = "Black Prison Population Percentage vs Total Prison Population")
  
  return (map)
}

map <- plot_map()