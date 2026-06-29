
library(dplyr)

##Exploración predictiva - WVS


#abro base
load("~/gefero-factor_data_silicon_sampling/data/wvs/F00010736-WVS_Cross-National_Wave_7_rdata_v6_0/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")

#dependientes
#Q199
#Q235
#Q237
#Q238
#Q239
#Q240

#independientes
#H_URBRURAL #Urban/Rural
#Q260 #Sex
#Q262 #Age
#Q269 #Are you a citizen of this country? 
#Q274 #Do you have any children?
#Q275 #What is the highest educational level that you, your spouse, your mother and your father have attained? (respondent)
#Q287 #People sometimes describe themselves as belonging to the working class, the middle class, or the upper or lower class. Would you describe yourself as belonging to the
#Q279 # Employment
#Q281 # Occupational groups


##modelo para analizar importancia

base = `WVS_Cross-National_Wave_7_v6_0`
rm(`WVS_Cross-National_Wave_7_v6_0`)

library(h2o)
h2o.init()

wvs_h2o <- as.h2o(base)

independientes <- c("B_COUNTRY","H_URBRURAL", "Q260", "Q262", "Q269", "Q274", "Q275", "Q287","Q279","Q281")
dependientes <- c("Q199", "Q235", "Q237", "Q238", "Q239", "Q240")


##INTERES EN LA POLÍTICA#######
# Q199: How interested would you say you are in politics? Are you...
modelo_rf_Q199 <- h2o.randomForest(
        x = independientes,
        y = "Q199",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q199)
h2o.varimp_plot(modelo_rf_Q199)

##SISTEMAS POLÍTICOS#####
# Q235: I'm going to describe various types of political systems and ask what you think 
#about each as a way of governing this country. 
#For each one, would you say it is a very good, fairly good, fairly bad or very bad way 
#of governing this country - 
#Having a strong leader who does not have to bother with parliament and elections

modelo_rf_Q235 <- h2o.randomForest(
        x = independientes,
        y = "Q235",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q235)
h2o.varimp_plot(modelo_rf_Q235)

##SISTEMAS POLÍTICOS 2#####
# Q236: I'm going to describe various types of political systems and ask what you think 
#about each as a way of governing this country. 
#For each one, would you say it is a very good, fairly good, fairly bad or very bad way 
#of governing this country - 
#Having experts, not government, make decisions according to what they think is best for the country

modelo_rf_Q236 <- h2o.randomForest(
        x = independientes,
        y = "Q236",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q236)
h2o.varimp_plot(modelo_rf_Q236)


##SISTEMAS POLÍTICOS#####
# Q237: I'm going to describe various types of political systems and ask what you think 
#about each as a way of governing this country. 
#For each one, would you say it is a very good, fairly good, fairly bad or very bad way 
#of governing this country - 
#Having the army rule

modelo_rf_Q237 <- h2o.randomForest(
        x = independientes,
        y = "Q237",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q237)
h2o.varimp_plot(modelo_rf_Q237)

##SISTEMAS POLÍTICOS#####
# Q238: I'm going to describe various types of political systems and ask what you think 
#about each as a way of governing this country. 
#For each one, would you say it is a very good, fairly good, fairly bad or very bad way 
#of governing this country - 
#Having a democratic political system

modelo_rf_Q238 <- h2o.randomForest(
        x = independientes,
        y = "Q238",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q238)
h2o.varimp_plot(modelo_rf_Q238)

# Q239: I'm going to describe various types of political systems and ask what you think 
#about each as a way of governing this country. 
#For each one, would you say it is a very good, fairly good, fairly bad or very bad way 
#of governing this country - 
#Having a system governed by religious law in which there are no political parties or elections

modelo_rf_Q239 <- h2o.randomForest(
        x = independientes,
        y = "Q239",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q239)
h2o.varimp_plot(modelo_rf_Q239)

## IDEOLOGÍA####
# Q240
#In political matters, people talk of "the left" and "the right." 
#How would you place your views on this scale, generally speaking?

modelo_rf_Q240 <- h2o.randomForest(
        x = independientes,
        y = "Q240",
        training_frame = wvs_h2o,
        ntrees = 200,
        max_depth = 20,
        seed = 1234
)

h2o.varimp(modelo_rf_Q240)
h2o.varimp_plot(modelo_rf_Q240)


##uno todo

imp_Q199 <- as.data.frame(h2o.varimp(modelo_rf_Q199)) %>%
        dplyr::mutate(dependiente = "Q199")

imp_Q235 <- as.data.frame(h2o.varimp(modelo_rf_Q235)) %>%
        dplyr::mutate(dependiente = "Q235")

imp_Q237 <- as.data.frame(h2o.varimp(modelo_rf_Q237)) %>%
        dplyr::mutate(dependiente = "Q237")

imp_Q238 <- as.data.frame(h2o.varimp(modelo_rf_Q238)) %>%
        dplyr::mutate(dependiente = "Q238")

imp_Q239 <- as.data.frame(h2o.varimp(modelo_rf_Q239)) %>%
        dplyr::mutate(dependiente = "Q239")

imp_Q240 <- as.data.frame(h2o.varimp(modelo_rf_Q240)) %>%
        dplyr::mutate(dependiente = "Q240")
imp_total <- bind_rows(imp_Q199, imp_Q235, imp_Q237, imp_Q238, imp_Q239, imp_Q240)

etiquetas <- c(
        "B_COUNTRY" = "Country",
        "H_URBRURAL" = "Urban/Rural",
        "Q260" = "Sex",
        "Q262" = "Age",
        "Q269" = "Citizen",
        "Q274" = "Children",
        "Q275" = "Education",
        "Q287" = "Subjective class",
        "Q279" = "Employment",
        "Q281" = "Occupational groups"
)

imp_total <- imp_total %>%
        mutate(variable = etiquetas[variable])

##plot

##etiqueto dep
etiquetas_dependientes <- c(
        "Q199" = "Interest in politics",
        "Q235" = "Strong leader without parliament",
        "Q236" = "Experts decide (no gov.)",
        "Q237" = "Army rule",
        "Q238" = "Democratic political system",
        "Q239" = "Religious law, no elections",
        "Q240" = "Left-Right political self-placement"
)


library(scales)
library(ggplot2)
g <- ggplot(imp_total, aes(x = reorder(variable, relative_importance), 
                           y = relative_importance)) +
        geom_col(fill = "steelblue") +
        facet_wrap(~ dependiente, scales = "free_y", 
                   labeller = labeller(dependiente = etiquetas_dependientes)) +
        coord_flip() +
        labs(title = "Importancia variables RF",
             x = "Variable independiente",
             y = "Importancia") +
        scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
        theme_minimal(base_size = 14) +
        theme(strip.text = element_text(size = 12),
              axis.text.x = element_text(size = 6),
              axis.title = element_text(size = 12))

ggsave("data/wvs/variable_importance_RF_WVS_2.pdf", plot = g, width = 12, height = 8, units = "in")



##Otras que podrían ser
##Q288 - ingreso percibido (o se solapará con clase)
##Q164 - religiosidad ??



