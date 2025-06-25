


library(dplyr)
library(tidyr)
library(countrycode)

df <- base

question_texts <- c(
        "Q199" = " How interested would you say you are in politics? Are you: Very interested, Somewhat interested, Not very interested, Not at all interested ",
        "Q235" = "I'm going to describe various types of political systems and ask what you think about each as a way of governing this country. For each one, would you say it is a very good, fairly good, fairly bad or very bad way of governing this country?: Having a strong leader who does not have to bother with parliament and elections",
        "Q236" = "I'm going to describe various types of political systems and ask what you think about each as a way of governing this country. For each one, would you say it is a very good, fairly good, fairly bad or very bad way of governing this country?: Having experts, not government, make decisions according to what they think is best for the country",
        "Q237" = "I'm going to describe various types of political systems and ask what you think about each as a way of governing this country. For each one, would you say it is a very good, fairly good, fairly bad or very bad way of governing this country?: Having the army rule",
        "Q238" = "I'm going to describe various types of political systems and ask what you think about each as a way of governing this country. For each one, would you say it is a very good, fairly good, fairly bad or very bad way of governing this country?: Having a democratic political system",
        "Q239" = "I'm going to describe various types of political systems and ask what you think about each as a way of governing this country. For each one, would you say it is a very good, fairly good, fairly bad or very bad way of governing this country?: Having a system governed by religious law in which there are no political parties or elections",
        "Q240" =  "In political matters, people talk of the left and the right. How would you place your views on this scale, generally speaking? 1 (Left) to 10 (Right)"
)

df <- df %>%
        mutate(
                id = row_number(),
                
                Country_label = countrycode(B_COUNTRY, origin = "iso3n", destination = "country.name", custom_match = c("858" = "Uruguay")),
                
                Sex_label = case_when(
                        Q260 == 1 ~ "man",
                        Q260 == 2 ~ "woman",
                        TRUE ~ "person of unspecified gender"
                ),
                
                Education_label = case_when(
                        Q275 %in% 0:1 ~ "low education",
                        Q275 %in% 2:3 ~ "secondary education",
                        Q275 %in% 4:6 ~ "higher education",
                        Q275 %in% 7:8 ~ "postgraduate education",
                        TRUE ~ "unspecified education"
                ),
                
                Class_label = case_when(
                        Q287 == 1 ~ "upper class",
                        Q287 == 2 ~ "upper middle class",
                        Q287 == 3 ~ "lower middle class",
                        Q287 == 4 ~ "working class",
                        Q287 == 5 ~ "lower class",
                        TRUE ~ "unspecified class"
                ),
                
                Occupation_label = case_when(
                        Q281 == 1 ~ "professional or technical worker",
                        Q281 == 2 ~ "higher administrative",
                        Q281 == 3 ~ "clerical worker",
                        Q281 == 4 ~ "sales worker",
                        Q281 == 5 ~ "service worker",
                        Q281 == 6 ~ "skilled worker",
                        Q281 == 7 ~ "semi-skilled worker",
                        Q281 == 8 ~ "unskilled worker",
                        Q281 == 9 ~ "farm worker",
                        Q281 == 10 ~ "farm proprietor or manager",
                        Q281 == 0 ~ "person who never had a job",
                        TRUE ~ "unspecified occupation"
                ),
                
                prompt = case_when(
                        Q281 == 0 ~ paste0(
                                "Imagine you are a ", Q262, "-year-old ", Sex_label,
                                ", who never had a job, with ", Education_label,
                                ", identifying as ", Class_label,
                                ", in ", Country_label, "."
                        ),
                        
                        TRUE ~ paste0(
                                "Imagine you are a ", Q262, "-year-old ", Sex_label,
                                ", working as a ", Occupation_label,
                                ", with ", Education_label,
                                ", identifying as ", Class_label,
                                ", in ", Country_label, "."
                        )
                )
        )


df_long <- crossing(
        id = df$id,
        dependiente = dependientes
) %>%
        left_join(df, by = "id") %>%
        mutate(
                question_text = question_texts[dependiente],
                full_prompt = paste0(prompt, " ", question_text)
        )


#ejemplos
# set.seed(123)
# df_long %>%
#         sample_n(10) %>%
#         select(dependiente, full_prompt) %>%
#         arrange(dependiente) %>%
#         pull(full_prompt) %>%
#         cat(sep = "\n\n-----\n\n")


df_prompt=df_long %>% select(id, dependiente, full_prompt) 

#guardo ambas
save(df_long, file="~/gefero-factor_data_silicon_sampling/data/wvs/df_long.RData")
save(df_prompt, file="~/gefero-factor_data_silicon_sampling/data/wvs/df_prompt.RData")




