library(tidyverse)

vci <- read_rds('./data/vcp-global-data.rds')

arg_2022 <- vci %>% 
        filter(Country == "Argentina" & Year == 2022) %>%
        mutate(age_llm = case_when(
                Age == "18-24" ~ "young",
                Age %in% c("25-34", "35-44") ~ "middle-aged",
                Age %in% c("45-54", "55-64") ~ "old",
                TRUE ~ "very old"),
               Education_Group = str_remove(Education_Group, " completed"),
               Income_grouped = tolower(Income_grouped),
               Income = case_when(
                       Income == "ABC1" ~ "upper-class",
                       Income == "C2" ~ "middle-class",
                       Income == "C3" ~ "middle-lower class",
                       Income == "D1D2E" ~ "very poor",
               ))

arg_2022 <- arg_2022 %>%
        mutate(history = paste0("I live in ", Country, ". My gender is ", tolower(Gender), 
                                ". In terms of my age, I am ", age_llm, 
                                ". Financially, I am ", 
                                Income, ". "))

arg_2022 %>% write_csv('./data/arg_2022_queries.csv')
