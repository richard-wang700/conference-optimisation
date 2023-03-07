# Made by: Richard Wang
# LinkedIn: linkedin.com/in/richard-wang700
# Github: github.com/richard7w

# Cleaning data from querying Wikipedia
library(lutz)
library(lubridate)
library(rvest)
library(tidyverse)
library(shiny)
rm(list=ls())

#Clean query data
wiki_query1 <- read_csv("private_university.csv")
wiki_query2 <- read_csv("public_university.csv")
wiki_query3 <- read_csv("research_university.csv")
wiki_query4 <- read_csv("university_university.csv")

# Additional option to add more data
# wiki_query_add <- read_csv("name.csv")

wiki_query_data <- wiki_query1 %>% 
  rbind(wiki_query2) %>% 
  rbind(wiki_query3) %>% 
  rbind(wiki_query4) #%>% 
  #rbind(wiki_query_add)

wiki_query_data <- wiki_query_data[!duplicated(wiki_query_data[c("item")]),]

wiki_query_data <- wiki_query_data %>% 
  select(name, location, countryLabel)
wiki_query_data$location <- str_replace_all(string = wiki_query_data$location, pattern = c('Point\\(' = "", '\\)' = ""))
wiki_query_data <- wiki_query_data %>% 
  separate(location, c("longitude", "latitude"), sep = " ")

cleaned_query_data <- wiki_query_data[!duplicated(wiki_query_data[c("longitude", "latitude")]),]
write_csv(cleaned_query_data, "cleaned_query_data.csv")
cleaned_query_data <- read_csv("cleaned_query_data.csv")
cleaned_query_data$city <- tz_lookup_coords(cleaned_query_data$latitude, cleaned_query_data$longitude, method = "fast")
cleaned_query_data$utc <- 1
n <- 1
for(x in cleaned_query_data$city){
  cleaned_query_data$utc[n] <- tz_offset("2023-01-01", x)$utc_offset_h
  n<- n+1
}

cleaned_query_data$latitude <- as.character(cleaned_query_data$latitude)
cleaned_query_data$longitude <- as.character(cleaned_query_data$longitude)
cleaned_query_data$utc <- as.character(cleaned_query_data$utc)
cleaned_query_data <- cleaned_query_data %>% 
  select(name, countryLabel, utc) %>% 
  rename("country" = "countryLabel",
         "university" = "name")
  

#-------------------------------------------------------------------------------
# Clean UTC timing data
utc_timings <- read_csv("UTC_Timings.csv")
utc_timings$UTC <- as.character(utc_timings$UTC)
utc_timings <- select(utc_timings, -26)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Quality of life data
qol_page <- read_html("https://en.wikipedia.org/wiki/Quality_of_life_index_by_country")
qol_raw <- html_node(qol_page, "#mw-content-text > div.mw-parser-output > table")
qol_processed <- html_table(qol_raw, fill = TRUE) %>% 
  as_tibble(.) %>% 
  select(-Rank)
colnames(qol_processed) <- c("Country", "QOL_Index")
qol_processed$QOL_Index <- as.character(qol_processed$QOL_Index)
qol_processed$Country[qol_processed$Country == 'United States'] <- 'United States of America'
qol_processed$Country[qol_processed$Country == 'China'] <- "People's Republic of China"
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Joining the two tables together
joint_tables <- cleaned_query_data %>%
  left_join(qol_processed, by = c("country" = "Country")) %>% 
  inner_join(utc_timings, by = c("utc" = "UTC"))

#Creates a file of the joined table
FINAL <- joint_tables %>% 
  drop_na(QOL_Index) %>% 
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else NA))
write_csv(FINAL, "complete_table.csv")
#-------------------------------------------------------------------------------

