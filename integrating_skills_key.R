library(tidyverse)
library(lubridate)
library(DataExplorer)

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")

# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

read_weather <- function (s) {
  ds <- read_csv(str_glue("us-weather-history/{s}.csv")) %>% 
    mutate(station = s, date = ymd(date))
}

ds <- read_weather("KCLT")
glimpse(ds)

# QUESTION 2
#> Use map_dfr() and your new function to read in all 10 stations
#> map_dfr() will take each dataframe and automatically bind them.
#> Save the resulting dataset to "ds"

ds <- map_dfr(stations, read_weather)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

ds$city <- factor(ds$station, levels = stations, labels = cities)
fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

f_to_c <- function(f) round((f-32)*5/9,1)
ds <- ds %>% mutate(across(actual_mean_temp:record_max_temp, f_to_c))

#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

extreme_days <- . %>% 
  mutate(is_extreme_day = actual_min_temp == record_min_temp | actual_max_temp == record_max_temp,
         is_extreme_day = as.numeric(is_extreme_day)) %>% 
  summarize(n_extreme = sum(is_extreme_day))

ds %>% group_by(city) %>% extreme_days %>% arrange(-n_extreme)

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

ds$month <- month(ds$date, label = T)
ds_by_month <- ds %>% group_by(month) %>% group_split %>% set_names(levels(ds$month))

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

for (df in ds_by_month) {
  precip_corr <- cor(df$average_precipitation, df$actual_precipitation)
  max_corr <- cor(df$average_max_temp, df$actual_max_temp)
  min_corr <- cor(df$average_min_temp, df$actual_min_temp)
  print(str_glue("{df$month[1]}: r(precip) = {format(precip_corr, digits = 2)}\t",
                                "r(min temp) = {format(min_corr, digits = 2)}\t",
                                "r(max temp) = {format(max_corr, digits = 2)}"))
}

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

plot_boxplot(ds, by = "city")
plot_boxplot(ds, by = "month")
plot_correlation(ds, type = "continuous")

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) + 
  geom_point() + 
  facet_wrap("city", ncol = 3)

# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

plot_month <- function(ds, m) {
  temp <- ds %>% filter(month == m)
  p <- ggplot(temp, aes(x = date, y = actual_mean_temp, color = city)) + 
    geom_point() + geom_line() + ggtitle(m)
  ggsave(str_glue("eda/{m}.png"))
  return(p)
}
map(unique(ds$month), ~ plot_month(ds, .x))

