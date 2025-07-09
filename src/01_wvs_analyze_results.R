library(jsonlite)
library(tidyverse)

import_profiles <- function(json_path) {
        # Leer JSON
        json_data <- fromJSON(json_path, simplifyVector = FALSE)
        
        # Extraer y desanidar
        profiles_list <- lapply(names(json_data), function(profile_id) {
                profile <- json_data[[profile_id]]
                
                tibble(
                        profile_id = profile_id,
                        character_profile = profile$character_profile,
                        survey_question = profile$survey_question,
                        think = profile$think,
                        answ = str_trim(profile$answ),  # Limpia espacios o saltos de línea
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
                        question_text = profile$profile_variables$question_text
                )
        })
        
        # Unir todo en un solo dataframe
        bind_rows(profiles_list)
}

uru_resp <- import_profiles("./data/wvs/resp/URU_Q199_survey_simulation_results.json")
arg_resp <- import_profiles("./data/wvs/resp/ARG_Q199_survey_simulation_results.json")

uru_resp  <- uru_resp  %>%
        mutate(
                response_extracted = str_split_i(uru_resp$response, "</think>",2)
                )

arg_resp  <- arg_resp  %>%
        mutate(
                response_extracted = str_split_i(arg_resp$response, "</think>",2)
        )

table(arg_resp$response_extracted)
