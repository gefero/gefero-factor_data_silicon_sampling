library(tidyverse)
library(broom)

# ── 1. Dicotomizar ────────────────────────────────────────────────────────────

# En datos sintéticos
resp <- resp %>%
        mutate(
                interesado = case_when(
                        survey_response %in% c("Very interested", "Somewhat interested") ~ 1,
                        survey_response %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_  # excluye INVALID FORMAT
                )
        )

# En datos reales WVS
wvs <- df %>%
        filter(B_COUNTRY_ALPHA %in% c("ARG", "URY", "USA")) %>%
        mutate(
                interesado = case_when(
                        Q199 %in% c("Very interested", "Somewhat interested") ~ 1,
                        Q199 %in% c("Not very interested", "Not at all interested") ~ 0,
                        TRUE ~ NA_real_
                )
        )

# ── 2. MAE  ──────────────────────────────────────────────────────────

# Proporciones REALES desde WVS
wvs_prop <- wvs %>%
        filter(!is.na(interesado)) %>%
        group_by(B_COUNTRY_ALPHA) %>%
        summarise(prop_real = mean(interesado))

# Proporciones SINTÉTICAS por modelo y país
llm_prop <- resp %>%
        filter(!is.na(interesado)) %>%
        group_by(llm_model, llm_size_agg, B_COUNTRY_ALPHA) %>%
        summarise(prop_llm = mean(interesado), .groups = "drop")

# MAE corregido
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


# ── 3. Regresión logística: datos reales (línea de base) ──────────────────────

modelo_base <- function(pais) {
        wvs_clean %>%
                filter(pais == !!pais) %>%
                glm(interesado ~ edad + sexo + educacion + clase,
                    data = ., family = binomial) %>%
                tidy(conf.int = TRUE) %>%
                mutate(country = pais, fuente = "WVS")
}

base_results <- map_dfr(c("ARG", "URY", "USA"), modelo_base)


# ── 5. Preparar datos sintéticos ─────────────────────────────────────────────

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



# ── 6. Regresión logística: datos sintéticos por modelo y país ───────────────

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

# ── 7. Coefficient plot ───────────────────────────────────────────────────────

coef_all <- bind_rows(
        base_results %>% 
                mutate(llm_model_size = "WVS"),  # agregás columna, no renombrás
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
                es_wvs = fuente == "WVS"  # ahora fuente existe en ambos
        )

coef_all_filtered <- coef_all %>%
        filter(abs(estimate) < 10,
               abs(conf.high - conf.low) < 20)

coef_all %>% select(fuente, es_wvs) %>% distinct() %>% print(n=20)

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
