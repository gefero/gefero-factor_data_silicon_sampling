library(jsonlite)
library(tidyverse)
library(gt)

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

tictoc::tic()
resp <- create_tibble("./data/wvs/resp/")
tictoc::toc()

resp <- resp %>%
        mutate(
                id = str_extract(profile_id, "(?<=profile_)\\d+"),
                llm_model = str_extract(profile_id, "(?<=_)[^_]+$"),
                llm_size = as.numeric(str_extract(llm_model,"(?<=:)(.*)(?=b)")) 
                ) %>%
        mutate(llm_size = case_when(
                llm_model == "gpt-4.1-mini"~ 15, 
                llm_model == "gemini-2.5-flash" ~ 25,
                llm_model == "gpt-4.1" ~ 176000,
                TRUE ~ llm_size)
               ) %>%
        mutate(llm_size_agg = case_when(
                llm_size <= 10 ~ "Small",
                llm_size > 10 & llm_size <=30 ~ "Medium",
                llm_size > 30 ~ "Large"
                )
        ) %>%
        mutate(llm_model = str_extract(llm_model, "^[^:]+")) %>% 
        select(profile_id, id, llm_model, llm_size, everything())

## Evaluación interna
## No respuesta
nrs <- resp %>%
        filter(survey_response == "[INVALID FORMAT OR UNRECOGNIZED OPTION]") 

resp %>%
        group_by(B_COUNTRY_ALPHA, llm_model, llm_size_agg, survey_response) %>%
        summarise(n=n()) %>%
        mutate(prop = 100*n/sum(n)) %>%
        filter(survey_response == "[INVALID FORMAT OR UNRECOGNIZED OPTION]")

### Distribución de respuestas

freqs_agg <- resp %>%
        mutate(llm_model_size = paste(llm_model, llm_size_agg, sep = "-")) %>%
        mutate(survey_response = if_else(
                survey_response == "[INVALID FORMAT OR UNRECOGNIZED OPTION]",
                "[Not valid format]",
                survey_response
        )) %>%
        group_by(B_COUNTRY_ALPHA, llm_model_size, survey_response) %>%
        summarise(n = n(), .groups = "drop_last") %>%  # Explicitly drops only last group
        mutate(prop = 100 * n / sum(n)) %>%
        ungroup()

freqs_agg %>%
        ggplot() + 
        geom_col(aes(x=llm_model_size, y=prop, fill=survey_response)) +
        theme_minimal() + 
        scale_fill_manual(
                values=c('#f5f5f5','#a6611a','#dfc27d','#80cdc1','#018571')
        )+
        coord_flip() +
        labs(y="%", x="Model - Size", fill="Interest in politics") +
        facet_wrap(~B_COUNTRY_ALPHA)

ggsave('./paper/plots/response_by_model_size.png',
       width = 8,
       height = 5,
       units = "in", # or "cm", "mm", or "px"
       dpi = 300 
       )

variability_metrics <- freqs_agg %>%
        group_by(B_COUNTRY_ALPHA, llm_model_size) %>%
        mutate(
                prop = 100 * n / sum(n),
                prob = prop / 100  # Probability (0-1)
        ) %>%
        summarise(
                # Sample information
                total_n = sum(n),
                n_categories = n(),
                
                # Entropy metrics
                entropy = -sum(prob * log2(prob)),
                max_entropy = log2(n_categories),
                normalized_entropy = entropy / log2(5),
                
                # Diversity indices
                simpson_diversity = 1 - sum(prob^2),
                effective_n_categories = 1 / sum(prob^2),
                
                # Gini-Simpson (alternative name, same as Simpson)
                gini_simpson = 1 - sum(prob^2),
                
                # Shannon diversity (entropy in nats instead of bits)
                shannon_diversity = -sum(prob * log(prob)),
                
                # Concentration metrics
                herfindahl_index = sum(prob^2),  # Inverse of Simpson
                concentration_ratio = max(prob),  # Largest proportion
                
                # Modal category information
                modal_response = survey_response[which.max(prop)],
                modal_pct = max(prop),
                
                # Distribution spread
                prop_sd = sd(prop),
                prop_cv = sd(prop) / mean(prop) * 100,  # Coefficient of variation
                
                # Inverse participation ratio (physics measure)
                ipr = 1 / sum(prob^4),
                
                .groups = "drop"
        ) %>%
        arrange(B_COUNTRY_ALPHA, llm_model_size)

variability_metrics %>% 
        select(B_COUNTRY_ALPHA:n_categories,
               normalized_entropy:effective_n_categories, 
               modal_pct, modal_response) %>%
        select(-total_n) %>%
        rename(Country = B_COUNTRY_ALPHA,
               `Total categories` = n_categories,
               `Model and size` = llm_model_size,
               `Norm. entropy` = normalized_entropy,
               `Simpson diversity` = simpson_diversity,
               `Effect. number of categories` = effective_n_categories,
               `Modal cat. percent.` = modal_pct,
               `Modal response` = modal_response)

variability_metrics %>% 
        select(B_COUNTRY_ALPHA,llm_model_size,
               normalized_entropy) %>%
        rename(Country = B_COUNTRY_ALPHA,
               `Model and size` = llm_model_size,
               `Norm. entropy` = normalized_entropy) %>%
        pivot_wider(
                names_from = Country,
                values_from = `Norm. entropy`
                ) %>%
        rowwise() %>% 
        mutate(Range = max(c_across(c(ARG,URY,USA)), na.rm = TRUE) 
               - min(c_across(c(ARG,URY,USA)), na.rm = TRUE)) %>%
        ungroup() %>%
        knitr::kable(digits=2) %>%
        writeLines('./paper/tables/norm_entropy.md')

variability_metrics %>% 
        select(B_COUNTRY_ALPHA,llm_model_size,
               effective_n_categories) %>%
        rename(Country = B_COUNTRY_ALPHA,
               `Model and size` = llm_model_size,
               `Effect. no. of categories` = effective_n_categories) %>%
        pivot_wider(
                names_from = Country,
                values_from = `Effect. no. of categories`
        ) %>%
        knitr::kable(digits=2) %>%
        writeLines('./paper/tables/eff_no_categories.md')

variability_metrics %>% 
        select(B_COUNTRY_ALPHA,llm_model_size,
               modal_pct, modal_response) %>%
        rename(Country = B_COUNTRY_ALPHA,
               `Model and size` = llm_model_size,
               `Modal cat. percent.` = modal_pct,
               `Modal response` = modal_response) %>%
        pivot_wider(
                names_from = Country,
                values_from = c(`Modal cat. percent.`,
                                `Modal response`)
        ) %>%
        knitr::kable(digits=2) %>%
        writeLines('./paper/tables/modal_statistcs.md')











library(tidyverse)
library(haven)

df <- read_rds('./data/wvs/WVS_Cross-National_Wave_7_rds_v6_0.rds')

knitr::kable(df %>%
        mutate(Q199 = as.character(haven::as_factor(Q199))) %>%
        filter(B_COUNTRY_ALPHA %in% c("URY", "ARG", "USA")) %>%
        filter(!Q199 %in% c("Don't know","No answer")) %>%
        group_by(B_COUNTRY_ALPHA, Q199) %>%
        summarise(n_w=sum(pwght)) %>%
        mutate(prop = n_w/sum(n_w),
               source = "WVS") %>%
        select(-n_w, -source) %>%
        pivot_wider(
                names_from = B_COUNTRY_ALPHA,
                values_from = prop,
                values_fill = 0
        ))



a#df %>%
#        left_join(
#                resp %>% 
#                        select(id,llm_model, survey_response) %>%
#                        pivot_wider(names_from = llm_model,
#                                    values_from = survey_response)
#        )

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
                        group_by(B_COUNTRY_ALPHA, llm_model, llm_size, llm_size_agg, survey_response) %>%
                        summarise(n=n()) %>%
                        mutate(prop = n/sum(n)) %>%
                        rename(Q199 = survey_response,
                               source = llm_model) 
        )
        
        
comp %>%
        filter(Q199 == "Interested" ) %>%
        ggplot() + 
                geom_line(aes(x=B_COUNTRY_ALPHA, 
                              y=mean(prop), 
                              group=llm_model, 
                              color=llm_size_agg)) +
                #scale_color_viridis_c() +
                ylim(0,1) +
                theme_minimal() +
                labs(x="País",
                     y="% rtas. 'Interesado + Algo interesado'",
                     color="Fuente")
                
        

