#Installing the packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('readr')
install.packages("scales")
#Loading the packages
library(tidyverse)
library(janitor)
library(lubridate)
library(readr)
library(scales)

#import data frames
divvy_trips_2019_q1 <- read_csv("raw_data/divvy_trips_2019_q1.csv")
divvy_trips_2019_q2 <- read_csv("raw_data/divvy_trips_2019_q2.csv")
divvy_trips_2019_q3 <- read_csv("raw_data/divvy_trips_2019_q3.csv")
divvy_trips_2019_q4 <- read_csv("raw_data/divvy_trips_2019_q4.csv")
#making copies of the data sets for working, and then removing original data sets
df_q1 <- divvy_trips_2019_q1
df_q2 <- divvy_trips_2019_q2
df_q3 <- divvy_trips_2019_q3
df_q4 <- divvy_trips_2019_q4
rm(divvy_trips_2019_q1)
rm(divvy_trips_2019_q2)
rm(divvy_trips_2019_q3)
rm(divvy_trips_2019_q4)

#becuase they all have differnt column names I will match then to the most convinien one
names(df_q1) <- names(df_q3)
names(df_q2) <- names(df_q3)
names(df_q4) <- names(df_q3)

#now I will full outer join all the data frames to one yearly data frame
df_tmp1 <- full_join(df_q1, df_q2)
df_tmp2 <- full_join(df_q3, df_q4)
df_2019 <- full_join(df_tmp1, df_tmp2)
rm(df_tmp1)
rm(df_tmp2)

#remove empty rows
df_2019 <- remove_empty(df_2019, which = c())

#addind day of the week column
df_2019 <- df_2019 %>% 
  mutate(ride_day = weekdays(start_time))

#adding the month of the ride
df_2019 <- df_2019 %>% 
  mutate(ride_month = month(start_time))

#now I will clean the df from irelevant data. includind trip duration between 0 and 7200 secconds(3 hours),
# I desided to limmit the max trip duration to 3 hour due to relevancy and making the visualisation easier to understad.
# in addition I will limit birth year to 1940 becuase realisticly no one over the age of 79 will be relevant eanogh 
# for the folowing years.
clean_df <- df_2019 %>% 
  filter(tripduration < 7200 & tripduration > 0 & birthyear > 1940)

#some of visualisation could be made in tableau so here is a way to downlad the cleaned data set.
write.csv(clean_df, file ='clean-data.csv')

#this is a plot that show rides distrebution acordint to days of the week
ggplot(clean_df) +
  geom_bar(mapping = aes(x = ride_day,  fill = usertype), position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(y = "Rides Count", x = "Day Of The Week", title = "Ride Count For Days Of The Week", fill = "Customer Type") +
  theme(axis.text.x = element_text(angle = 30)) 

#this plot is the same but for months
ggplot(clean_df) +
  geom_bar(mapping = aes(x = month.abb[ride_month],  fill = usertype), position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(y = "Rides Count", x = "Month Of The Year", title = "Ride Count For Months Of The Year", fill = "Customer Type")

#now a plot that shows the diffrance between birthyears
ggplot(clean_df) +
  geom_bar(mapping = aes(x = birthyear,  fill = usertype), position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(y = "Rides Count", x = "Birth Year", title = "Ride Count For Different Birth Years", fill = "Customer Type")

#and now different duration to the persantage of subscribers
df_tmp <- clean_df %>% 
  mutate(usertype_numeric = ifelse(usertype == "Subscriber", 1, 0)) %>% 
  mutate(tripduration_10_sec = ceiling(tripduration / 10) * 10)
df_tmp <- df_tmp %>% 
  group_by(tripduration_10_sec) %>% 
  summarise(usertype = mean(usertype_numeric), ride_num = n())
ggplot(df_tmp) +
  geom_point(mapping = aes(x = tripduration_10_sec,  y = ride_num, colour = usertype )) +
  scale_color_gradient(low = 'yellow', high = 'purple') +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(y = "Count Of Rides", x = "Trip Duration In Seconds", title = "Count Of Rides VS Trip Duration", colour = "Subscriber Ratio")
ggplot(df_tmp) +
  geom_smooth(mapping = aes(x = tripduration_10_sec,  y = ride_num), colour = 'darkgreen') +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(y = "Count Of Rides", x = "Trip Duration In Seconds", title = "Count Of Rides VS Trip Duration")