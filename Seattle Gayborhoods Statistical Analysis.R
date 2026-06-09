#Gayborhoods 2015-2026 data

library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)

seattle_gayborhoods <- read.csv("https://raw.githubusercontent.com/maryspra/gayborhoods/refs/heads/main/Seattle_gayborhoods_2015_2026.csv",stringsAsFactors = FALSE)

#How many gay bars in Seattle for 2026?
#Answer: 20
X2026_total_SEA_bars <- seattle_gayborhoods %>%
  summarize(sum(X2026_bars))

#How many gay bars in Seattle for 2015?
#Answer:19

X2015_total_SEA_bars <- seattle_gayborhoods %>%
  summarize(sum(X2015_bars))



#2026 Seattle: What are the top 7 zip codes for gay bars?
SEA_top_gaybars_2026 <- seattle_gayborhoods %>% 
  slice_max(X2026_bars,n=7) %>%  #Top 7
  mutate(ZIP_code = as.character(ZIP_code)) %>%   #read zip codes as text
  mutate(Neighborhoods = c(   
    "Capitol Hill",
    "Downtown",
    "Capitol Hill",
    "Wallingford",
    "White Center",
    "Beacon Hill",
    "Bremerton"))   #Assign neighborhood labels per zip code


ggplot(SEA_top_gaybars_2026) + 
  geom_col(aes (x = ZIP_code,
                y = X2026_bars,
                fill = Neighborhoods)) +
  labs(title = "2026: Seattle Zip Codes with the Highest Number of Gay Bars",
       x = "ZIP Code",
       y = "Number of Gay Bars",
       fill = "Neighborhoods")    

#2015 Seattle: What are the top 7 zip codes for gay bars?
SEA_top_gaybars_2015 <- seattle_gayborhoods %>% 
  slice_max(X2015_bars,n=7) %>%  #Top 7
  mutate(ZIP_code = as.character(ZIP_code)) %>%   #read zip codes as text
  mutate(Neighborhoods = c(   
    "Capitol Hill/ Central District",
    "Downtown/ First Hill",
    "Capitol Hill/ Eastlake",
    "Wallingford",
    "Capitol Hill/ Montlake/ Madison",
    "West Seattle/ Fauntleroy",
    "Beacon Hill/ Central District/ Mt. Baker"))   #Assign neighborhood labels per zip code


ggplot(SEA_top_gaybars_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_bars,
                fill = Neighborhoods)) +
  labs(title = "2015: Seattle Zip Codes with the Highest Number of Gay Bars",
       x = "ZIP Code",
       y = "Number of Gay Bars",
       fill = "Neighborhoods")    


#_________________________________________________________________________

#2024 Seattle: Which zip codes have the highest number of same sex couples?

SEA_top_population_2024 <- seattle_gayborhoods %>% 
  slice_max(X2024_same_sex_couples,n=7) %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Capitol Hill/ Central District",
    "Capitol Hill/ Eastlake",
    "Wallingford/ Fremont/ Greenwood",
    "Columbia City/ Rainier Beach/ Seward Park",
    "Queen Anne/ SLU",
    "Northgate/Lake City",
    "Beacon Hill/Central District/ Mt. Baker"))


ggplot(SEA_top_population_2024) + 
  geom_col(aes (x = ZIP_code,
                y = X2024_same_sex_couples,
                fill = Neighborhoods)) +
  labs(title = "2024: Seattle ZIP Codes with the Highest Number of Same-sex Couples",
       x = "ZIP Code",
       y = "Number of Same Sex Couples",
       fill = "Neighborhoods")  

#2015 Seattle: Which zip codes have the highest number of same sex couples?

SEA_top_population_2015 <- seattle_gayborhoods %>% 
  slice_max(X2015_same_sex_couples,n=7) %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Capitol Hill/ Central District",
    "Beacon Hill/ Central District/ Mt. Baker",
    "Capitol Hill/ Eastlake",
    "Roosevelt/ Wedgewood/ Ravenna",
    "West Seattle",
    "Wallingford/ Fremont/ Greenwood",
    "Columbia City/ Rainier Beach/ Seward Park"))


ggplot(SEA_top_population_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_same_sex_couples,
                fill = Neighborhoods)) +
  labs(title = "2015: Seattle ZIP Codes with the Highest Number of Same-sex Couples",
       x = "ZIP Code",
       y = "Number of Same Sex Couples",
       fill = "Neighborhoods")

#__________________________________________________________________________________________

#2024 Seattle: Which zip codes have the highest rate of same sex couples per zip code?
#remove zip codes with total households below 3000 to avoid statistical outliers

SEA_minus_smallest_zipcodes_2024 <- seattle_gayborhoods %>% 
  slice_max(X2024_households_all,n=88) 

SEA_top_rate_per_1000_2024 <- SEA_minus_smallest_zipcodes_2024 %>%
  slice_max(X2024_gay_couples_per_1000_household,n=10)  %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "Capitol Hill/ Central District",
    "Capitol Hill/ Montlake/ Madison",
    "Capitol Hill/ Eastlake",
    "Vashon Island",
    "West Seattle/ Alki",
    "West Seattle/ Fauntleroy",
    "Beacon Hill/Central District/ Mt. Baker",
    "Columbia City/ Seward Park/ Rainier Beach",
    "First Hill/ International District/ Pioneer Square",
    "West Seattle/ White Center"))


ggplot(SEA_top_rate_per_1000_2024) + 
  geom_col(aes (x = ZIP_code,
                y = X2024_gay_couples_per_1000_household,
                fill = Neighborhoods)) +
  labs(title = "2024: Seattle ZIP Codes with the Highest Rate of Same-sex Couples per 1000 Households",
       x = "ZIP Code",
       y = "Rate of Same Sex Couples per 1000 Households",
       fill = "Neighborhoods")  

#2015 Seattle: Which zip codes have the highest rate of same sex couples per zip code?
#remove zip codes with total households below 3000 to avoid statistical outliers

SEA_minus_smallest_zipcodes_2015 <- seattle_gayborhoods %>% 
  slice_max(X2015_households_all,n=88) 

SEA_top_rate_per_1000_2015 <- SEA_minus_smallest_zipcodes_2015 %>%
  slice_max(X2015_gay_couples_per_1000_household,n=10)  %>%
  mutate(ZIP_code = as.character(ZIP_code)) %>%
  mutate(Neighborhoods = c(
    "West Seattle",
    "Capitol Hill/ Central District",
    "Beacon Hill/ Central District/ Mt.Baker",
    "Capitol Hill/ Eastlake",
    "Downtown/ First Hill",
    "West Seattle/ Fauntleroy",
    "First Hill/ International District/ Pioneer Square",
    "Capitol Hill/ Montlake/ Madison",
    "Skyway/Tukwila/Renton",
    "Beacon Hill/ Georgetown/ South Park"))


ggplot(SEA_top_rate_per_1000_2015) + 
  geom_col(aes (x = ZIP_code,
                y = X2015_gay_couples_per_1000_household,
                fill = Neighborhoods)) +
  labs(title = "2015: Seattle ZIP Codes with the Highest Rate of Same-sex Couples per 1000 Households, 2015",
       x = "ZIP Code",
       y = "Rate of Same Sex Couples per 1000 Households, 2015",
       fill = "Neighborhoods")  

#_____________________________________________________________________________________________________

#2024-2026 Seattle: Which zip codes make top rankings across multiple metrics?
SEA_gayest_neighborhoods_2024_2026 <- rbind(SEA_top_gaybars_2026, SEA_top_population_2024, SEA_top_rate_per_1000_2024)

SEA_gayest_zipcodes_2024_2026 <-SEA_gayest_neighborhoods_2024_2026 %>%
  group_by(ZIP_code) %>%
  summarize(number_of_occurences=n()) %>%
  slice_max(number_of_occurences, n=6)

#2015 Seattle: Which zip codes make top rankings across multiple metrics?

SEA_gayest_neighborhoods_2015 <- rbind(SEA_top_gaybars_2015, SEA_top_population_2015, SEA_top_rate_per_1000_2015)

SEA_gayest_zipcodes_2015 <-SEA_gayest_neighborhoods_2015 %>%
  group_by(ZIP_code) %>%
  summarize(number_of_occurences=n()) %>%
  slice_max(number_of_occurences, n=8)





#NYC_gayborhoods <- read.csv("https://raw.githubusercontent.com/maryspra/gayborhoods/refs/heads/main/Seattle_gayborhoods_2015_2026.csv",stringsAsFactors = FALSE)
