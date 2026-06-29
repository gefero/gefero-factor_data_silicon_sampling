library(tidyverse)
library(haven)

df <- read_rds('./data/latinobarometro/F00017013-Latinobarometro_2023_Rds_v1_0/Latinobarometro_2023_Eng_Rds_v1_0.rds')

extract_labels <- function(x){
        if (is.labelled(x)){
                return(as.character(as_factor(x)))
        } else {
                return(x)
        }
}

df <- df %>% mutate(across(everything(), extract_labels))

library(FactoMineR)
library(factoextra)
library(ggrepel)
mca_ict <- df %>%
        select(starts_with("P18")) %>%
        mutate(across(everything(), ~case_when(
                .x %in% c("Agree", "Strongly agree") ~ "Agree",
                TRUE ~ "Not agree"
        ))) %>%
        MCA(X = ., ncp = 4, row.w = df$wt, graph = FALSE)

fviz_eig(mca_ict)              

mca_ict$var$coord %>%
        as_tibble(rownames = "var") %>%
        ggplot(aes(x = `Dim 1`, y = `Dim 3`)) + 
        geom_point() + 
        geom_text_repel(aes(label = var), max.overlaps = 20, size = 3) + 
        ylim(-3, 6) + 
        theme_minimal()
