library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(RColorBrewer)

bnb = read.csv('./AB_NYC_2019.csv')

bnb = bnb %>% mutate(p_radius = ifelse(price <= 100, 1,
                                       ifelse(
                                         price <= 500, 2, ifelse(price <= 1000, 10, ifelse(
                                           price <= 3000, 20, ifelse(price <= 10000, 30, price)
                                         ))
                                       ))) %>%
  mutate(r_radius = ifelse(
    reviews_per_month <= 1,
    1,
    ifelse(
      reviews_per_month <= 5,
      2,
      ifelse(
        reviews_per_month <= 10,
        10,
        ifelse(reviews_per_month <= 58.5, 15, reviews_per_month)
      )
    )
  )) %>%
  mutate(b_radius = ifelse(
    number_of_reviews <= 100,
    1,
    ifelse(
      number_of_reviews <= 200,
      2,
      ifelse(
        number_of_reviews <= 300,
        3,
        ifelse(number_of_reviews <= 629, 15, number_of_reviews)
      )
    )
  ))

