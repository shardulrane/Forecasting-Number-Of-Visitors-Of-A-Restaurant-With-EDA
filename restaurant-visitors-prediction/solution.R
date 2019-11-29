setwd("C:/User/DELL/Desktop/restaurant-visitors-dataset")

library(data.table)
library(magrittr)
library(ggplot2)
library(lubridate)
library(xts)
library(forecast)
library(prophet)
library(DT)
library(gridExtra)
library(leaflet)
library(htmltools)
library(mapdata)
library(maptools)
library(sp)
library(dplyr)
library(fpp2)
library(plyr)
library(tidyr)
library(purrr)
library(tidyquant)
library(timetk)
library(sweep)
library(urca)
air_reserve <- fread("air_reserve.csv")
write.csv(air_reserve)
air_store <- fread("air_store_info.csv")
air_visit <- fread("air_visit_data.csv")
date_info <- fread("date_info.csv")
hpg_reserve <- fread("hpg_reserve.csv")
hpg_store <- fread("hpg_store_info.csv")
store_id <- fread("store_id_relation.csv")
test <- fread("sample_submission.csv")

# studying individual data
dim(air_store)
str(air_store)
head(air_store) 
uniqueN(air_store$air_store_id)


dim(hpg_store)
str(hpg_store)
head(hpg_store) 
uniqueN(hpg_store$hpg_store_id)

dim(air_reserve)
str(air_reserve)
head(air_reserve) 
air_reserve[, `:=` (visit_datetime = as.Date(visit_datetime), 
                    reserve_datetime = as.Date(reserve_datetime)
)
]
head(air_reserve[, .(.N), by = visit_datetime][order(-N)], 20)

head(air_reserve[, .(.N), by = reserve_datetime][order(-N)], 20)
uniqueN(air_reserve$air_store_id)


dim(hpg_reserve) 
str(hpg_reserve)
head(hpg_reserve) 
hpg_reserve[, `:=` (visit_datetime = as.Date(visit_datetime), 
                    reserve_datetime = as.Date(reserve_datetime)
)
]
head(hpg_reserve[, .(.N), by = visit_datetime][order(-N)], 20)
head(hpg_reserve[, .(.N), by = reserve_datetime][order(-N)], 20)
uniqueN(hpg_reserve$hpg_store_id)

dim(store_id) 
str(store_id)
head(store_id) 
uniqueN(store_id$air_store_id) 
uniqueN(store_id$hpg_store_id)

dim(air_visit) 
str(air_visit)
head(air_visit)
air_visit[, visit_date := as.Date(visit_date)]
head(air_visit[, .(.N), by = visit_date][order(-N)], 20)
uniqueN(air_visit$air_store_id)
summary(sort(unique(air_visit$air_store_id)) == sort(unique(air_store$air_store_id)))
ggAcf(air_visit$visitors, lag = 10000)
air_visit$visitors %>% ur.kpss() %>% summary
ndiffs(air_visit$visitors)
dim(date_info)
str(date_info)
head(date_info) 
holidays <- date_info[holiday_flg == 1, ]



empty(air_store)
## [1] FALSE
empty(hpg_store)
## [1] FALSE
empty(air_reserve)
## [1] FALSE
empty(hpg_reserve)
## [1] FALSE
empty(store_id)
## [1] FALSE
empty(air_visit)
## [1] FALSE
empty(date_info)
## [1] FALSE


anyNA(air_store)
## [1] FALSE
anyNA(hpg_store)
## [1] FALSE
anyNA(air_reserve)
## [1] FALSE
anyNA(hpg_reserve)
## [1] FALSE
anyNA(store_id)
## [1] FALSE
anyNA(air_visit)
## [1] FALSE
anyNA(date_info)
## [1] FALSE
air_store[, .(.N), by = air_genre_name][order(-N)]

air_ggplot <- air_store[, .(.N), by = air_genre_name] %>%
  ggplot(aes(x = reorder(air_genre_name, N), y = N)) +
  geom_bar(stat = 'identity', fill = 'dark grey') +
  labs(x = '', y = 'Number of stores', title = 'Air System Restaurant') + 
  coord_flip()
#for hpg
hpg_store[, .(.N), by = hpg_genre_name][order(-N)]

hpg_ggplot <- hpg_store[, .(.N), by = hpg_genre_name] %>%
  ggplot(aes(x = reorder(hpg_genre_name, N), y = N)) +
  geom_bar(stat = 'identity', fill = 'dark grey') +
  labs(x = '', y = 'Number of stores', title = 'HPG System Restaurant') + 
  coord_flip()

grid.arrange(air_ggplot, hpg_ggplot, ncol = 2)

#Comparing between air and hpg store location ke hisaab se karke dekhte hai

air_store[, .(.N), by = air_area_name][order(-N)]
air_ggplot <- air_store[, .(.N), by = air_area_name] %>%
  head(5) %>%
  ggplot(aes(x = reorder(air_area_name, N), y = N)) +
  geom_bar(stat = 'identity', fill = 'dark grey') +
  labs(x = '', y = 'Number of stores', title = 'Air System Restaurant') + 
  coord_flip()

# HPG restaurants

hpg_store[, .(.N), by = hpg_area_name][order(-N)]
hpg_ggplot <- hpg_store[, .(.N), by = hpg_area_name] %>%
  head(5) %>%
  ggplot(aes(x = reorder(hpg_area_name, N), y = N)) +
  geom_bar(stat = 'identity', fill = 'dark grey') +
  labs(x = '', y = 'Number of stores', title = 'HPG System Restaurant') + 
  coord_flip()

grid.arrange(air_ggplot, hpg_ggplot, nrow = 2)


#air aur hpg ko reservation ke hisaab se compare karke dekhte hai

air_ggplot <- air_reserve[, .(number_reservations = .N), by = .(date = as.Date(visit_datetime))] %>%
  ggplot(aes(x = date, y = number_reservations)) + 
  geom_line(color = 'dark blue') + 
  geom_vline(data = holidays, aes(xintercept = as.Date(calendar_date)), alpha = 0.4) + 
  scale_x_date(date_breaks = "2 weeks") + 
  labs(x = '', y = 'Number of reservations', title = 'Air restaurants reservations') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

hpg_ggplot <- hpg_reserve[, .(number_reservations = .N), by = .(date = as.Date(visit_datetime))] %>%
  ggplot(aes(x = date, y = number_reservations)) + 
  geom_line(color = 'dark blue') +
  geom_vline(data = holidays, aes(xintercept = as.Date(calendar_date)), alpha = 0.4) + 
  scale_x_date(date_breaks = "2 weeks") + 
  labs(x = '', y = 'Number of reservations', title = 'HPG restaurants reservations') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(air_ggplot, hpg_ggplot, nrow = 2)

# air resturants ke hisaab se visits dekhte hai

plot1 <- air_visit[, .(total_visitors = sum(visitors)), by = visit_date] %>%
  ggplot(aes(x = visit_date, y = total_visitors)) + 
  geom_line(color = 'dark blue') + 
  geom_vline(data = holidays, aes(xintercept = as.Date(calendar_date)), alpha = 0.4) + 
  scale_x_date(date_breaks = "2 weeks") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = '', y = 'Total visits', title = 'Daily Visits for Air System Restaurants with Holidays')

plot2 <- air_visit[order(wday(visit_date)), .(mean_visits = mean(visitors)), by = weekdays(visit_date)] %>%
  ggplot(aes(x = reorder(weekdays, seq(1,7)), y = mean_visits)) + 
  geom_bar(stat = 'identity', fill = 'dark grey') +
  labs(x = '', y = "Average visits", title = "Average visits by day of week for Air system restaurants")


grid.arrange(plot1, plot2, nrow = 2)

#to start forecasting
nchar(test[1,1])

head(test)

test[, `:=` (air_store_id = substr(test$id, 1,20)
             , visit_date = as.Date(substr(test$id, 22,31))
)
]
test[, "visitors"] = NULL
head(test)

air_visit_arima <- air_visit %>%
  group_by(air_store_id) %>%
  nest(.key = "data.tbl")
visit_date=air_visit$visit_date
air_visit_arima_ts <- air_visit_arima %>%
  mutate(data.ts = map(.x = data.tbl, 
                       .f = tk_ts, 
                       select = -visit_date,
                       start = 2016-01-13))

head(air_visit_arima_ts)

air_visit_arima_fit <- air_visit_arima_ts %>%
  mutate(fit.arima = map(data.ts, auto.arima)) 

head(air_visit_arima_fit)



air_visit_arima <- air_visit %>%
  group_by(air_store_id) %>%
  nest(.key = "data.tbl")

air_visit_arima_ts <- air_visit_arima %>%
  mutate(data.ts = map(.x = data.tbl, 
                       .f = tk_ts, 
                       select = -visit_date,
                       start = 2016-01-13))

head(air_visit_arima_ts)

air_visit_arima_fit <- air_visit_arima_ts %>%
  mutate(fit.arima = map(data.ts, auto.arima)) 

head(air_visit_arima_fit)


