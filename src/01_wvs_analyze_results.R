library(jsonlite)
library(tidyverse)

import_profiles <- function(json_path) {
        # Leer JSON
        json_data <- fromJSON(json_path, simplifyVector = FALSE)
        
        profiles_list <- lapply(names(json_data), function(profile_id) {
                profile <- json_data[[profile_id]]
                
                tibble(
                        profile_id = profile_id,
                        character_profile = profile$character_profile,
                        think_process = profile$think_process,
                        survey_response = str_trim(profile$survey_response),
                        id = profile$profile_variables$id,
                        D_INTERVIEW = profile$profile_variables$D_INTERVIEW,
                        B_COUNTRY_ALPHA = profile$profile_variables$B_COUNTRY_ALPHA,
                        Country_label = profile$profile_variables$Country_label,
                        Age = profile$profile_variables$Age,
                        Sex_label = profile$profile_variables$Sex_label,
                        Education_label = profile$profile_variables$Education_label,
                        Class_label = profile$profile_variables$Class_label,
                        Occupation_label = profile$profile_variables$Occupation_label,
                        dependiente = profile$profile_variables$dependiente,
                        question_text = profile$profile_variables$question_text,
                        valid_options = profile$profile_variables$valid_options
                )
        })
        
        bind_rows(profiles_list)
}

## Importamos y apilamos las respuestas de LLM
#resp <- import_profiles(json_path='./data/wvs/resp/URY_prompt1v3_Q199_deepseek1b.json') %>%
#        bind_rows(import_profiles(json_path='./data/wvs/resp/ARG_prompt1v3_Q199_deepseek1b.json')) %>%
#        bind_rows(import_profiles(json_path='./data/wvs/resp/USA_prompt1v3_Q199_deepseek1b.json'))


create_tibble <- function(path="./data/wvs/resp/"){
        resp <- tibble()
        for (f in list.files(path, pattern=".json", full.names = TRUE)){
                r <- import_profiles(json_path=f)
                resp <- resp %>% bind_rows(r)
        }
        
        return(resp)
}


resp <- create_tibble("./data/wvs/resp/")

resp <- resp %>%
        mutate(
                id = str_extract(profile_id, "(?<=profile_)\\d+"),
                llm_model = str_extract(profile_id, "(?<=_)[^_]+$")
                ) %>%
        select(profile_id, id, llm_model, everything())

## Para evaluar la No respuesta
nrs <- resp %>%
        filter(survey_response == "[INVALID FORMAT OR UNRECOGNIZED OPTION]") 

resp %>%
        group_by(B_COUNTRY_ALPHA, llm_model, survey_response) %>%
        summarise(n=n()) %>%
        mutate(prop = n/sum(n)) %>%
        filter(survey_response == "[INVALID FORMAT OR UNRECOGNIZED OPTION]")

## Importamos y procesamos data de WVS

library(tidyverse)
library(haven)

df <- read_rds('./data/wvs/WVS_Cross-National_Wave_7_rds_v6_0.rds') %>%
        mutate(across(c(B_COUNTRY_ALPHA, Q199), ~as_factor(.x)))



df %>%
        left_join(
                resp %>% 
                        select(id,llm_model, survey_response) %>%
                        pivot_wider(names_from = llm_model,
                                    values_from = survey_response)
        )

## Código horrendo que genera la tabla apilada de las dos fuentes
comp <- df %>%
        filter(B_COUNTRY_ALPHA %in% c("URY", "ARG", "USA")) %>% # filtra los países
        mutate(Q199 = if_else(Q199 %in% c("No answer", "Don't know"),
                              "No data", Q199)
        ) %>% # construye una categoría "No data"
        filter(Q199 != "No data") %>% # filtra
        mutate(Q199 = if_else(
                Q199 %in% c("Not at all interested", "Not very interested"),
                "Not interested", "Interested")) %>% # dicotomiza la variable
        group_by(B_COUNTRY_ALPHA, Q199) %>%
        summarise(n=n()) %>%
        mutate(prop = n/sum(n),
               source = "WVS") %>%
        bind_rows(
                resp %>%
                        filter(survey_response != "[INVALID FORMAT OR UNRECOGNIZED OPTION]") %>%
                        mutate(survey_response = if_else(
                                survey_response %in% c("Not at all interested", "Not very interested"),
                                "Not interested", "Interested")) %>%
                        group_by(B_COUNTRY_ALPHA, llm_model, survey_response) %>%
                        summarise(n=n()) %>%
                        mutate(prop = n/sum(n)) %>%
                        rename(Q199 = survey_response,
                               source = llm_model) 
        )
        
        
comp %>%
        filter(Q199 == "Interested" ) %>%
        ggplot() + 
                geom_line(aes(x=B_COUNTRY_ALPHA, 
                              y=prop, 
                              group=source, 
                              color=source)) +
                ylim(0,1) +
                theme_minimal() +
                labs(x="País",
                     y="% rtas. 'Interesado + Algo interesado'",
                     color="Fuente")


