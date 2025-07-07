library(tidyverse)
#load('./data/wvs/df_prompt.RData')
load('./data/wvs/df_long.RData')

df_long %>%
        rename(Age = Q262) %>%
        select(id,D_INTERVIEW, B_COUNTRY_ALPHA, Country_label, 
               Age, Sex_label:Occupation_label, dependiente, question_text) %>%
        group_split(dependiente) %>% 
        purrr::walk(function(df_long) {
                nombre_grupo <- unique(df_long$dependiente)
                write_csv(df_long, paste0("./data/wvs/", nombre_grupo, ".csv"))
        }
        )
        
        


set.seed(123)
df_long %>%
        select(id,D_INTERVIEW, B_COUNTRY_ALPHA, Country_label, dependiente, full_prompt) %>%
        group_by(Country_label, dependiente) %>%
        sample_n(5) %>%
        ungroup() %>%
        group_split(dependiente) %>% 
        purrr::walk(function(df_long) {
                nombre_grupo <- unique(df_long$dependiente)
                write_csv(df_long, paste0("./data/wvs/sample_prompts_", nombre_grupo, ".csv"))
        }
        )


df_long %>%
        filter(
                Country_label %in% c("Argentina","United States", "Uruguay","United Kingdom") & 
                        dependiente == "Q199") %>%
        group_by(Country_label) %>%
        summarise(n=n())


df_long %>%
        sample_n(10) %>%
        select(full_prompt) %>%
        pull()

x <- read_csv('./data/wvs/prompts_Q199.csv')

# lista_por_grupo <- df_long %>%
#         group_split(dependiente) %>%
#         set_names(map_chr(., ~ unique(.x$dependiente))) %>%
#         map(~ select(.x, -dependiente))  # opcional: quitar la columna 'dependiente' dentro de cada grupo
# 
# # Convertir a JSON
# json_resultado <- toJSON(lista_por_grupo, pretty = TRUE, auto_unbox = TRUE)
