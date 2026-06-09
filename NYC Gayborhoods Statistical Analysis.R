#Gayborhoods 2015-2026 data

library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)

NYC_gayborhoods <- read.csv("https://raw.githubusercontent.com/maryspra/gayborhoods/refs/heads/main/NYC_gayborhoods_2015_2026.csv",stringsAsFactors = FALSE)

#How many gay bars in NYC for 2026?

X2026_total_NYC_bars <- NYC_gayborhoods %>%
  summarize(sum(X2026_bars))

#How many gay bars in NYC for 2015?


X2015_total_NYC_bars <- NYC_gayborhoods %>%
  summarize(sum(X2015_bars))



#2026 NYC: What are the top 7 zip codes for gay bars?
NYC_top_gaybars_2026 <- NYC_gayborhoods %>% 
  slice_max(X2026_bars,n=9) %>%  #zip codes with 3 or more
  mutate(ZIP_code = as.character(ZIP_code)) %>%   #read zip codes as text
  mutate(Neighborhoods = c(   
    "West Village",
    "Hell's Kitchen",
    "Hell's Kitchen/ Times Square",
    "East Village/ Gramercy Park",
    "Williamsburg",
    "Chelsea",
    "Jackson Heights", 
    "Harlem/ Morningside Heights",
    "Bushwick/ Bed-Stuy/ Williamsburg"
    ))   #Assign neighborhood labels per zip code


ggplot(NYC_top_gaybars_2026) + 
  geom_col(aes (x = ZIP_code,
                y = X2026_bars,
                fill = Neighborhoods)) +
  labs(title = "2026: NYC Zip Codes with the Highest Number of Gay Bars",
       x = "ZIP Code",
       y = "Number of Gay Bars",
       fill = "Neighborhoods")    

#2015 NYC: What are the top 7 zip codes for gay bars?
NYC_top_gaybars_2015 <- NYC_gayborhoods %>% 
  slice_max(X2015_bars,n=5) %>%  #Zip codes with 3 or more
  mutate(ZIP_code = as.character(ZIP_code)) %>%   #read zip codes as text
  mutate(Neighborhoods = c(   
    "West Village",
    "Hell's Kitchen",
    "Hell's Kitchen/ Times Square",
    "Chelsea",
    "East Village/ Gramercy Park"))   #Assign neighborhood labels per zip code


ggplot(NYC_top_gaybars_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_bars,
                fill = Neighborhoods)) +
  labs(title = "2015: NYC Zip Codes with the Highest Number of Gay Bars",
       x = "ZIP Code",
       y = "Number of Gay Bars",
       fill = "Neighborhoods")    


#_________________________________________________________________________

#2024 NYC: Which zip codes have the highest number of same sex couples?

NYC_top_population_2024 <- NYC_gayborhoods %>% 
  slice_max(X2024_same_sex_couples,n=7) %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Chelsea",
    "East Village/ Gramercy Park",
    "Hell's Kitchen",
    "Upper West Side",
    "Park Slope",
    "Downtown Brooklyn/ Brooklyn Heights",
    "Hell's Kitchen/ Times Square"))


ggplot(NYC_top_population_2024) + 
  geom_col(aes (x = ZIP_code,
                y = X2024_same_sex_couples,
                fill = Neighborhoods)) +
  labs(title = "2024: NYC ZIP Codes with the Highest Number of Same-sex Couples",
       x = "ZIP Code",
       y = "Number of Same Sex Couples",
       fill = "Neighborhoods")  

#2015 NYC: Which zip codes have the highest number of same sex couples?

NYC_top_population_2015 <- NYC_gayborhoods %>% 
  slice_max(X2015_same_sex_couples,n=7) %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Chelsea",
    "Upper West Side",
    "Upper West Side/ Manhattan Valley",
    "West Village",
    "Williamsburg",
    "Upper West Side",
    "Hell's Kitchen"))


ggplot(NYC_top_population_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_same_sex_couples,
                fill = Neighborhoods)) +
  labs(title = "2015: NYC ZIP Codes with the Highest Number of Same-sex Couples",
       x = "ZIP Code",
       y = "Number of Same Sex Couples",
       fill = "Neighborhoods")

#__________________________________________________________________________________________

#2024 NYC: Which zip codes have the highest rate of same sex couples per zip code?
#remove zip codes with total households below 5000 to avoid statistical outliers

NYC_minus_smallest_zipcodes_2024 <- NYC_gayborhoods %>% 
  slice_max(X2024_households_all,n=327) 

NYC_top_rate_per_1000_2024 <- NYC_minus_smallest_zipcodes_2024 %>%
  slice_max(X2024_gay_couples_per_1000_household,n=10)  %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Hell's Kitchen/ Times Square",
    "West Village",
    "East Village/ Gramercy Park",
    "Chelsea",
    "Hell's Kitchen",
    "Financial District",
    "Carroll Gardens/ Red Hook",
    "Park Slope",
    "Washington Heights",
    "Prospect Heights"))


ggplot(NYC_top_rate_per_1000_2024) + 
  geom_col(aes (x = ZIP_code,
                y = X2024_gay_couples_per_1000_household,
                fill = Neighborhoods)) +
  labs(title = "2024: NYC ZIP Codes with the Highest Rate of Same-sex Couples per 1000 Households",
       x = "ZIP Code",
       y = "Rate of Same Sex Couples per 1000 Households",
       fill = "Neighborhoods")  

#2015 NYC: Which zip codes have the highest rate of same sex couples per zip code?
#remove zip codes with total households below 3000 to avoid statistical outliers

NYC_minus_smallest_zipcodes_2015 <- NYC_gayborhoods %>% 
  slice_max(X2015_households_all,n=315) 

NYC_top_rate_per_1000_2015 <- NYC_minus_smallest_zipcodes_2015 %>%
  slice_max(X2015_gay_couples_per_1000_household,n=10)  %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Chelsea",
    "Hell's Kitchen/ Times Square",
    "Upper Westside/ Lincoln Square",
    "West Village",
    "Chelsea",
    "Hell's Kitchen",
    "Nyack, Orangetown",
    "Carroll Gardens/ Red Hook",
    "Jersey City",
    "Upper Westside"))


ggplot(NYC_top_rate_per_1000_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_gay_couples_per_1000_household,
                fill = Neighborhoods)) +
  labs(title = "2015: NYC ZIP Codes with the Highest Rate of Same-sex Couples per 1000 Households",
       x = "ZIP Code",
       y = "Rate of Same Sex Couples per 1000 Households, 2015",
       fill = "Neighborhoods")  

#_____________________________________________________________________________________________________

#2024-2026 NYC: Which zip codes hold top rankings across multiple metrics?
NYC_gayest_neighborhoods_2024_2026 <- rbind(NYC_top_gaybars_2026, NYC_top_population_2024, NYC_top_rate_per_1000_2024)

NYC_gayest_zipcodes_2024_2026 <-NYC_gayest_neighborhoods_2024_2026 %>%
  group_by(ZIP_code) %>%
  summarize(number_of_occurences=n()) %>%
  slice_max(number_of_occurences, n=6) #zipcodes that occur twice or more in the rankings

#2015 NYC: Which zip codes make top rankings across multiple metrics?

NYC_gayest_neighborhoods_2015 <- rbind(NYC_top_gaybars_2015, NYC_top_population_2015, NYC_top_rate_per_1000_2015)

NYC_gayest_zipcodes_2015 <-NYC_gayest_neighborhoods_2015 %>%
  group_by(ZIP_code) %>%
  summarize(number_of_occurences=n()) %>%
  slice_max(number_of_occurences, n=6) #zipcodes that occur twice or more in the rankings
