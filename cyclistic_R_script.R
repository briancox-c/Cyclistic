# load packages

packages <- c("here", "tidyverse", "data.table", "kableExtra")

install.packages(setdiff(packages, rownames(installed.packages())))

library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(data.table, quietly = TRUE)

# Set some defaults

options(scipen = 9)

theme_set(theme_minimal())

theme_update(legend.position = "none", plot.margin = margin(20,5,20,5))

blues <- c(member = "lightblue", casual = "navy")

month_labels <- c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec")

# read in and compile monthly tables into one yearly table

setwd(here("data/monthly"))

year_data <-
        list.files() %>% 
        lapply(function(x) fread(x)) %>% 
        rbindlist()

setwd(here())

# preview the data

str(year_data)

summary(year_data)

# rename ambiguously named columns

colnames(year_data)[3] <- "time_start"

colnames(year_data)[4] <- "time_end"

# investigate NA values

colSums(is.na(year_data))

number_na_end_coords <-
        nrow(year_data[is.na(end_lng) | is.na(end_lat), ])

percent_na_end_coords <-
        number_na_end_coords / nrow(year_data) * 100

cat(c("Records with missing end coordinates are",
      round(percent_na_end_coords, 2),
      "% of the total records"))

# remove NA values

year_data <-
        year_data %>% 
        filter(!is.na(end_lat) & !is.na(end_lng))

# investigate suspicious zero values

number_zero_end_coords <-
        nrow(year_data[end_lng == 0 | end_lat == 0, ])

cat(c("There are",
      number_zero_end_coords,
      "records with zero for an ending longitude or latitude"))

view(year_data[end_lng == 0 | end_lat == 0, ])

# investigate test records

tests <-
        year_data[grepl("test", year_data$start_station_name,
                        ignore.case = TRUE) |
                  grepl("test", year_data$end_station_name,
                        ignore.case = TRUE) |
                  grepl("test", year_data$start_station_id,
                        ignore.case = TRUE) |
                  grepl("test", year_data$end_station_id,
                        ignore.case = TRUE), ]

nrow(tests)

view(tests)

# remove test records and records with zero end coordinates

year_data <-
        year_data %>% 
        filter(end_lat != 0 & end_lng !=0,
               !grepl("test", start_station_name, ignore.case = TRUE),
               !grepl("test", end_station_name, ignore.case = TRUE),
               !grepl("test", start_station_id, ignore.case = TRUE),
               !grepl("test", end_station_id, ignore.case = TRUE))

# verify primary key uniqueness

length(unique(year_data$ride_id)) == nrow(year_data)

# look for invalid values in low-cardinality fields

unique(year_data$rideable_type)

unique(year_data$member_casual)

# find and investigate empty-string values

year_data %>%
        select_if(is.character) %>%
        sapply(function(x) sum(x == ""))

percent_empty_start_id <-
        round(sum(year_data$start_station_id == "") / nrow(year_data) * 100, 2)

percent_empty_end_id <- 
        round(sum(year_data$end_station_id == "") / nrow(year_data) * 100, 2)
        
cat(paste(percent_empty_start_id,
      "% of the start station ids are empty, and",
    percent_empty_end_id, 
      "% of the end station ids are empty"))

# replace empty strings

year_data <-
        year_data %>% 
        mutate(
                start_station_id = case_when(
                        start_station_id == '' ~ paste(start_lat,start_lng),
                        .default = as.character(start_station_id)),
                end_station_id = case_when(
                        end_station_id == '' ~ paste(end_lat,end_lng),
                        .default = as.character(end_station_id))
)

year_data <- 
        year_data %>% 
        mutate(
                start_station_name = case_when(
                        start_station_name == '' ~ start_station_id,
                        .default = as.character(start_station_name)),
                end_station_name = case_when(
                        end_station_name == '' ~ end_station_id,
                        .default = as.character(end_station_name))
)

# create duration variable and investigate start and end times

year_data$duration_mins <-
        as.numeric(difftime(year_data$time_end,
                            year_data$time_start, units = "mins"))

summary(year_data$duration_mins)

na_durations <- sum(is.na(year_data$duration_mins))

negative_durations <- sum(year_data$duration_mins < 0)

zero_durations <- sum(year_data$duration_mins == 0)

dayplus_durations <- sum(year_data$duration_mins >= 1440)

strange_duration_report <- data.table(
        na_durations, negative_durations,
        zero_durations, dayplus_durations
)

percent_strange_durations <-
        sum(na_durations, negative_durations, zero_durations,
        dayplus_durations) / nrow(year_data) * 100

strange_duration_report

cat("records with strange durations are",
    round(percent_strange_durations, 2), "% of the total records")

# remove records with invalid durations

year_data <-
        year_data[year_data$duration_mins > 0 &
        year_data$duration_mins < 1440, ]

# re-summarize duration variable to verify no remaining anomalies

na_durations <- sum(is.na(year_data$duration_mins))

negative_durations <- sum(year_data$duration_mins < 0)

zero_durations <- sum(year_data$duration_mins == 0)

dayplus_durations <- sum(year_data$duration_mins >= 1440)

min_duration <- min(year_data$duration_mins) %>% round(4)

avg_duration <- mean(year_data$duration_mins) %>% round(2)

median_duration <- median(year_data$duration_mins) %>% round(2)

percentile_99.9 <- quantile(year_data$duration_mins, probs = 0.999) %>% round(1)

max_duration <- max(year_data$duration_mins) %>% round(1)

clean_duration_report <-
        data.table(na_durations, negative_durations, zero_durations,
                   dayplus_durations, min_duration, median_duration,
                   avg_duration, percentile_99.9, max_duration)

clean_duration_report

# plot duration distribution to check for reasonableness

ggplot(year_data, aes(x = duration_mins)) +
        geom_histogram(binwidth = 15) +
        xlim(0, 1500)

ggplot(year_data, aes(x = duration_mins, fill = member_casual)) +
        geom_histogram(binwidth = 5) +
        xlim(0, 100) +
        scale_fill_manual(values = blues) +
        facet_wrap(member_casual ~ .)

# take a 10% sample of the data

set.seed(0)

smp <- slice_sample(year_data, prop = 0.1)

# export tables for use in SQL, Tableau, and Excel
# (commented out to prevent writing files during review)

# fwrite(smp[!"duration_mins"],
#        here("data/yearly/sampled_yearly_data.csv"))
# 
# fwrite(year_data[!"duration_mins"],
#        here("data/yearly/clean_whole_year.csv"))

# remove temporary variables to keep environment tidy

rm(
        number_na_end_coords, percent_na_end_coords, number_zero_end_coords,
        na_durations, negative_durations, zero_durations, dayplus_durations,
        strange_duration_report, percent_strange_durations, min_duration,
        avg_duration, median_duration, percentile_99.9, max_duration,
        clean_duration_report, percent_empty_start_id, percent_empty_end_id,
        tests, year_data
)

# add calculated fields for expedient analysis

smp <- smp %>% 
        mutate(day_type =
                       case_when(wday(time_start) %in% c(1, 7) ~ "Weekend",
                                 wday(time_start) %in% c(2, 6) ~
                                         "Shoulder Weekday",
                                 wday(time_start) %in% 3:5 ~ "Middle Weekday"),
               trip_type =
                       case_when(paste(start_lat, start_lng) ==
                                         paste(end_lat, end_lng) ~ "Round Trip",
                                 .default = "One Way"),
               hour_start = hour(time_start),
               month_start = month(time_start)
)

# check calculated fields for reasonableness

table(smp$member_casual, smp$day_type)

table(smp$member_casual, smp$trip_type)

table(smp$member_casual, smp$hour_start)

table(smp$member_casual, smp$month_start)

# (I used BigQuery to calculate and export the distance and direction
# of each ride using the following query)

# DROP TABLE IF EXISTS cyclistic.geo;

# CREATE TABLE cyclistic.geo AS

#     SELECT

#         ride_id,

#         (ST_DISTANCE(ST_GEOGPOINT(end_lng, end_lat),

#                ST_GEOGPOINT(start_lng, start_lat)))

#                * 0.0006213712 ## convert meters to miles

#                 AS distance_miles,

#         CAST(ST_AZIMUTH(ST_GEOGPOINT(start_lng, start_lat),

#                 ST_GEOGPOINT(end_lng, end_lat))

#                 * 57.29578 AS INT64) ## convert radians to degrees

#                 AS direction

#     FROM cyclistic.smp

#     WHERE trip_type = 'One Way';

# read in the direction and distance data from BigQuery

geo <- fread(here("data/yearly/geo_calc_vars.csv"))

# exploratory analysis: plot some time-clustering variables

ggplot(smp, aes(day_type, after_stat(prop), group = member_casual,
                fill = member_casual,
                label = paste(round(after_stat(prop)*100),"%"))) +
        geom_bar() +
        labs(title = "Percent of Rides by Day Type",
             subtitle = "Members rode more midweek, non-members weekends") +
        geom_text(stat = "count", color = "white", nudge_y = -0.05) +
        scale_y_continuous(label = scales::percent) +
        theme(axis.text.y=element_blank()) +
        scale_fill_manual(values = blues) +
        facet_grid(vars(member_casual))

ggplot(smp, aes(hour_start, after_stat(prop), fill = member_casual)) +
        geom_bar() +
        labs(title = "Percent of Rides by Hour and Day Type",
             subtitle = "Members had a bigger spike during the morning commute") +
        scale_y_continuous(label = scales::percent) +
        scale_fill_manual(values = blues) +
        facet_grid(vars(member_casual), vars(day_type))

ggplot(smp, aes(month_start, after_stat(prop), fill = member_casual)) +
        geom_bar() +
        labs(title = "Percent of Rides by Month and Day Type",
             subtitle = "Non-members' usage was more seasonal") +
        scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12),
                           labels = month_labels) +
        scale_y_continuous(label = scales::percent) +
        scale_fill_manual(values = blues) +
        facet_grid(vars(member_casual), vars(day_type))

# exploratory analysis: plot some ride direction variables

dir_mwd_morning <- smp %>%
        right_join(geo, by = join_by(ride_id)) %>% 
        filter(day_type == 'Middle Weekday',
               hour_start %in% 5:8,
               distance_miles > 1)

dir_mwd_evening <- smp %>%
        right_join(geo, by = join_by(ride_id)) %>% 
        filter(day_type == 'Middle Weekday',
               hour_start %in% 15:18,
               distance_miles > 1)

ggplot(dir_mwd_morning, aes(direction, after_stat(prop), group = member_casual,
                            fill = member_casual)) +
        geom_bar() +
        labs(title = "Direction of Rides During Midweek Morning Commute") +
        scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) +
        scale_y_continuous(label = scales::percent) +
        scale_fill_manual(values = blues) +
        facet_grid(vars(member_casual))

ggplot(dir_mwd_evening, aes(direction, after_stat(prop), group = member_casual,
                            fill = member_casual)) +
        geom_bar() +
        labs(title = "Direction of Rides During Midweek Evening Commute") +
        scale_x_continuous(breaks = c(0, 90, 180, 270, 360)) +
        scale_y_continuous(label = scales::percent) +
        scale_fill_manual(values = blues) +
        facet_grid(vars(member_casual))

# exploratory analysis: plot some ride duration and distance variables

smp %>% right_join(geo, by = join_by(ride_id)) %>%
        filter(rideable_type != 'docked_bike') %>%
        group_by(member_casual) %>% 
        summarise(median_duration_mins = median(duration_mins) %>% round(1),
                  median_distance_mi = median(distance_miles) %>% round(2),
                  median_speed_mph = median(distance_miles /
                                     (duration_mins / 60)) %>% round(1))

smp %>% right_join(geo, by = join_by(ride_id)) %>%
        filter(rideable_type != 'docked_bike',
               day_type != 'Shoulder Weekday') %>%
        group_by(day_type, member_casual) %>% 
        summarise(median_duration_mins = median(duration_mins) %>% round(1),
                  median_distance_mi = median(distance_miles) %>% round(2),
                  median_speed_mph = median(distance_miles /
                                     (duration_mins / 60)) %>% round(1))

quantile(geo$distance_miles, 0.9) %>% round(2)
   
dist_box <- smp %>%
        select(ride_id, member_casual, day_type, rideable_type) %>%
        right_join(geo, by = join_by(ride_id)) %>%
        filter(distance_miles > 0 & distance_miles <= 2.87,
               day_type != 'Shoulder Weekday',
               rideable_type != 'docked_bike'
)

ggplot(dist_box, aes(rideable_type, distance_miles)) +
        geom_boxplot() +
        labs(title = "Median Distance of Rides by Bike Type and Day Type",
             subtitle = "Members used electric bikes for longer distances, but non-members did not") +
        facet_grid(vars(member_casual), vars(day_type))


quantile(smp$duration_mins, 0.9) %>% round(2)

dur_box <- smp %>% 
        filter(duration_mins > 0 & duration_mins <= 29.03,
               day_type != "Shoulder Weekday")

ggplot(dur_box, aes(member_casual, duration_mins)) +
        geom_boxplot() +
        labs(title = "Median Duration of Rides by Day Type",
             subtitle = "Non-members rode for longer times, especially on weekends") +
        facet_grid(vars(day_type))

ggplot(dur_box, aes(member_casual, duration_mins)) +
        geom_boxplot() +
        labs(title = "Median Duration of Rides by Day Type and Month",
             subtitle = "The biggest difference was in the summer") +
        scale_x_discrete(labels = c("c", "m")) +
        facet_grid(vars(day_type), vars(month_start))
