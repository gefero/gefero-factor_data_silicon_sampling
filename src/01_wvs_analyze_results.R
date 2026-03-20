library(jsonlite)
library(tidyverse)
library(gt)
library(effectsize)
library(broom)
library(purrr)
library(knitr)

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




#5.1 - error absoluto medio entre WVS y silicon sampling

wvs_freq <- resp %>%
        group_by(B_COUNTRY_ALPHA, survey_response) %>%
        summarise(n=n()) %>%
        mutate(prop_real = n/sum(n))

llm_freq <- freqs_agg %>%
        mutate(prop_llm = prop/100)

aggregate_mae <- llm_freq %>%
        left_join(wvs_freq, by=c("B_COUNTRY_ALPHA","survey_response")) %>%
        group_by(llm_model_size, B_COUNTRY_ALPHA) %>%
        summarise(
                MAE = mean(abs(prop_llm - prop_real), na.rm=TRUE)
        )


aggregate_mae_plot <- aggregate_mae %>%
        group_by(llm_model_size) %>%
        mutate(mae_mean_model = mean(MAE, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
                llm_model_size = fct_reorder(llm_model_size, mae_mean_model, .desc = TRUE)
        )

ggplot(aggregate_mae_plot,
                aes(x = MAE, y = llm_model_size, color = B_COUNTRY_ALPHA)) +
        geom_point(size = 3, alpha = 0.9, position = position_dodge(width = 0.4)) +
        scale_color_manual(
                values=c('#a6611a','#dfc27d','#80cdc1','#018571')
        )+
        labs(
                x = "MAE",
                y = "Model - Size",
                color = "Country"
        ) +  theme_minimal(base_size = 12)


ggsave(
        "./paper/plots/mae_by_model_country.png",
        width = 8,
        height = 5,
        units = "in",
        dpi = 300
)



##tabla modelos 4.3: 
##agrega tipo


resp %>% 
        distinct(llm_model, llm_size, llm_size_agg) %>%
        arrange(llm_size) %>%
        mutate(
                model_type = case_when(
                        llm_model %in% c("gpt-4.1", "gpt-4.1-mini", "gemini-2.5-flash") ~ "Proprietary",
                        TRUE ~ "Open-weight"
                ),
                llm_model = case_when(
                        llm_model == "deepseek-r1" ~ "DeepSeek-R1",
                        llm_model == "llama3.1" ~ "Llama 3.1",
                        llm_model == "qwen3" ~ "Qwen 3",
                        llm_model == "gpt-4.1-mini" ~ "GPT-4.1 Mini",
                        llm_model == "gemini-2.5-flash" ~ "Gemini 2.5 Flash",
                        llm_model == "gpt-4.1" ~ "GPT-4.1",
                        TRUE ~ llm_model
                ),
                llm_size = case_when(
                        llm_model == "GPT-4.1" ~ "~175B",
                        llm_model == "GPT-4.1 Mini" ~ "~15B",
                        llm_model == "Gemini 2.5 Flash" ~ "~25B",
                        TRUE ~ paste0(llm_size, "B")
                )
        ) %>%
        rename(
                Model = llm_model,
                `Model size` = llm_size,
                Category = llm_size_agg,
                Type = model_type
        ) %>%
        select(Model, Type, `Model size`, Category) %>%
        knitr::kable(
                caption = "Large language models used in the analysis, model type, approximate parameter size, and size category."
        ) %>%
        writeLines("./paper/tables/models_table.md")



##5.2 subgrupos

## Dicotómico

#LLMs

resp <- resp %>%
        mutate(
                interesado = case_when(
                        survey_response %in% c("Very interested", "Somewhat interested") ~ 1,
                        survey_response %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_  # excluye INVALID FORMAT
                )
        )

#WVS
wvs <- df %>%
        filter(B_COUNTRY_ALPHA %in% c("ARG", "URY", "USA")) %>%
        mutate(
                interesado = case_when(
                        Q199 %in% c("Very interested", "Somewhat interested") ~ 1,
                        Q199 %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_
                )
        )


# Proporciones desde WVS

wvs_prop <- wvs %>%
        filter(!is.na(interesado)) %>%
        group_by(B_COUNTRY_ALPHA) %>%
        summarise(prop_real = mean(interesado))

llm_prop <- resp %>%
        filter(!is.na(interesado)) %>%
        group_by(llm_model, llm_size_agg, B_COUNTRY_ALPHA) %>%
        summarise(prop_llm = mean(interesado), .groups = "drop")

# MAE 
mae_corrected <- llm_prop %>%
        left_join(wvs_prop, by = "B_COUNTRY_ALPHA") %>%
        mutate(
                MAE = abs(prop_llm - prop_real),
                llm_model_size = paste(llm_model, llm_size_agg, sep = "-")
        )



ggplot(mae_corrected,
       aes(x = MAE, y = llm_model_size, color = B_COUNTRY_ALPHA)) +
        geom_point(size = 3, alpha = 0.9, position = position_dodge(width = 0.4)) +
        scale_color_manual(
                values=c('#a6611a','#dfc27d','#80cdc1','#018571')
        )+
        labs(
                x = "MAE",
                y = "Model - Size",
                color = "Country"
        ) +  theme_minimal(base_size = 12)


ggsave(
        "./paper/plots/mae_by_model_country.png",
        width = 8,
        height = 5,
        units = "in",
        dpi = 300
)




wvs_clean <- df %>%
        filter(B_COUNTRY_ALPHA %in% c("ARG", "URY", "USA")) %>%
        mutate(
                interesado = case_when(
                        as.character(Q199) %in% c("Very interested", "Somewhat interested") ~ 1,
                        as.character(Q199) %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_
                ),
                edad = as.numeric(Q262),
                sexo = as.numeric(Q260),
                educacion = as.numeric(Q275),
                clase = as.numeric(Q287),
                pais = as.character(B_COUNTRY_ALPHA)
        ) %>%
        filter(!is.na(interesado))%>% select(B_COUNTRY_ALPHA,interesado,edad,sexo,
                                             educacion , clase, pais)


# Regresión WVS

modelo_base <- function(pais) {
        wvs_clean %>%
                filter(pais == !!pais) %>%
                glm(interesado ~ edad + sexo + educacion + clase,
                    data = ., family = binomial) %>%
                tidy(conf.int = TRUE) %>%
                mutate(country = pais, fuente = "WVS")
}

base_results <- map_dfr(c("ARG", "URY", "USA"), modelo_base)


#LLMs

resp_clean <- resp %>%
        mutate(
                interesado = case_when(
                        survey_response %in% c("Very interested", "Somewhat interested") ~ 1,
                        survey_response %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_
                ),
                edad = as.numeric(Age),
                # Recodificar para que coincidan las escalas con WVS
                sexo = case_when(
                        Sex_label == "man"   ~ 1,
                        Sex_label == "woman" ~ 2,
                        TRUE ~ NA_real_
                ),
                educacion = case_when(
                        Education_label == "low education"            ~ 1,
                        Education_label == "secondary education"      ~ 2,
                        Education_label == "higher education"         ~ 3,
                        Education_label == "postgraduate education"   ~ 4,
                        Education_label == "unspecified education"    ~ NA_real_,
                        TRUE ~ NA_real_
                ),
                clase = case_when(
                        Class_label == "lower class"        ~ 1,
                        Class_label == "working class"      ~ 2,
                        Class_label == "lower middle class" ~ 3,
                        Class_label == "upper middle class" ~ 4,
                        Class_label == "upper class"        ~ 5,
                        TRUE ~ NA_real_
                ),
                pais = B_COUNTRY_ALPHA,
                llm_model_size = paste(llm_model, llm_size_agg, sep = "-")
        ) %>%
        filter(!is.na(interesado))


# Regresión llmS


modelos_llm <- resp_clean %>%
        group_by(llm_model_size, pais) %>%
        group_modify(~ {
                tryCatch(
                        glm(interesado ~ edad + sexo + educacion + clase,
                            data = .x, family = binomial) %>%
                                tidy(conf.int = TRUE),
                        error = function(e) tibble()  # si algún modelo falla, skip
                )
        }) %>%
        ungroup() %>%
        rename(country = pais) %>%
        mutate(fuente = llm_model_size)


coef_all <- bind_rows(
        base_results %>% 
                mutate(llm_model_size = "WVS"),  
        modelos_llm %>% 
                mutate(llm_model_size = fuente)
) %>%
        filter(term != "(Intercept)") %>%
        mutate(
                term = recode(term,
                              "edad"      = "Age",
                              "sexo"      = "Sex",
                              "educacion" = "Education",
                              "clase"     = "Social class"
                ),
                es_wvs = fuente == "WVS"  
        )

coef_all_filtered <- coef_all %>%
        filter(abs(estimate) < 10,
               abs(conf.high - conf.low) < 20)

coef_all %>% select(fuente, es_wvs) %>% distinct() %>% print(n=20)

#Gráfico

ggplot(coef_all_filtered, aes(x = estimate, y = llm_model_size, 
                              color = es_wvs, alpha = es_wvs)) +
        geom_point(size = 2.5) +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
        facet_grid(term ~ country, scales = "free_x") +
        scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#018571"),
                           labels = c("FALSE" = "LLMs", "TRUE" = "WVS")) +
        coord_cartesian(xlim = c(-3, 3))+
        scale_alpha_manual(values = c(0.5, 1), guide = "none") +
        labs(x = "Coef (log-odds)", y = NULL, color = NULL)
theme_minimal(base_size = 11) +
        theme(legend.position = "bottom",
              strip.text = element_text(face = "bold"))


ggsave(
        "./paper/plots/regresion_model_country.png",
        width = 12,
        height = 10,
        units = "in",
        dpi = 300
)











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

df %>% 
        filter(B_COUNTRY_ALPHA %in% c("ARG", "URY", "USA")) %>%
        select(B_COUNTRY_ALPHA, Q199, starts_with("Q260"), starts_with("Q262"), 
               starts_with("Q275"), starts_with("Q287"), starts_with("Q288")) %>%
        head(3)

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
                
        


