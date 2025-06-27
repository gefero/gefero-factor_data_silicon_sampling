library(tidyverse)
library(jsonlite)
#load('./data/wvs/df_prompt.RData')
load('./data/wvs/df_long.RData')

df_long %>%
        select(id,D_INTERVIEW, B_COUNTRY_ALPHA, Country_label, dependiente, full_prompt) %>%
        group_split(dependiente) %>% 
        purrr::walk(function(df_long) {
                nombre_grupo <- unique(df_long$dependiente)
                write_csv(df_long, paste0("./data/wvs/prompts_", nombre_grupo, ".csv"))
        }
        )


x <- read_csv('./data/wvs/prompts_Q199.csv')

# lista_por_grupo <- df_long %>%
#         group_split(dependiente) %>%
#         set_names(map_chr(., ~ unique(.x$dependiente))) %>%
#         map(~ select(.x, -dependiente))  # opcional: quitar la columna 'dependiente' dentro de cada grupo
# 
# # Convertir a JSON
# json_resultado <- toJSON(lista_por_grupo, pretty = TRUE, auto_unbox = TRUE)
