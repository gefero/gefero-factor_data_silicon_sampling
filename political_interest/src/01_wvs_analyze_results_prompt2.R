library(tidyverse)
library(googledrive)

folder_url <- c("https://drive.google.com/drive/u/0/folders/115fSD-r4NtvxrigpOtHb-3p8DMRyyAgI")


load_csvs_from_drive_folders <- function(folder_urls) {
        drive_auth()
        
        extract_folder_id <- function(url) {
                m <- regmatches(url, regexpr("(?<=folders/)[a-zA-Z0-9_-]+", url, perl = TRUE))
                if (length(m) == 0) stop(paste("Could not parse folder ID from URL:", url))
                m
        }
        
        read_folder <- function(folder_id) {
                files <- drive_ls(as_id(folder_id), type = "text/csv")
                
                if (nrow(files) == 0) {
                        message("No CSV files found in folder: ", folder_id)
                        return(list())
                }
                
                lapply(seq_len(nrow(files)), function(i) {
                        #tmp <- tempfile(fileext = ".csv")
                        #drive_download(as_id(files$id[i]), path = tmp, overwrite = TRUE)
                        #read_csv(tmp, show_col_types = FALSE)
                        raw <- drive_read_string(as_id(files$id[i]))
                        read_csv(I(raw), show_col_types = FALSE)
                        
                })
        }
        
        folder_ids <- vapply(folder_urls, extract_folder_id, character(1), USE.NAMES = FALSE)
        all_dfs    <- unlist(lapply(folder_ids, read_folder), recursive = FALSE)
        
        if (length(all_dfs) == 0) stop("No CSV files found across all folders.")
        
        bind_rows(all_dfs)
}
df <- load_csvs_from_drive_folders("https://drive.google.com/drive/u/0/folders/115fSD-r4NtvxrigpOtHb-3p8DMRyyAgI")

df <- df %>%
        mutate(model = str_remove_all(model, "[^[:alnum:][:space:]]")) %>%
       #mutate(model = tolower(model)) %>%
        mutate(empirical = tolower(empirical)) %>%
        mutate(model = case_when(
                str_detect(model, "Not Very interested|Not Very Interested|not very interested|Id be quite surprised if I said Very interested  my life is too mundane to waste on politics and considering my background leftleaning doesnt always translate to a passionate interest") ~ "Not very interested",
                str_detect(model, "I am very interested") ~ "Very interested",
                #str_detect(model, "very interesting|very interested") ~ "Very interested",
                str_detect(tolower(model), "not at all interesting|very not interested|very not interested|very uninterested|not at all|im not really that into politics its just something i have to worry about when i need a job or something not really my thing") ~ "Not at all interested",
                str_detect(tolower(model), "somewhatinterested|somewhat interesting|somewhat interest|somewhat|im not particularly passionate about politics but i have a moderate interest i would not be deeply involved unless something important were to happen") ~ "Somewhat interested",
                model == "Very interesting" ~ "Very interested",
                model == "very interesting" ~ "Very interested",
                model == "Very interesting"~ "Very interested",
                model == "very interested" ~ "Very interested",
                model == "Very Interested" ~ "Very interested",
                model == "exact option string" ~ NA_character_,
                TRUE ~ model)
        ) %>%
        mutate(model = tolower(model))


df <- df %>%
        pivot_longer(model:empirical,
                     names_to="variable",
                     values_to="response")


model_dist <- df %>%
        group_by(model_name, country, variable, response) %>%
        summarise(n = n()) %>%
        drop_na() %>%
        mutate(prop = n/sum(n)) %>%
        ungroup() %>%
        complete(model_name, country, variable, response, fill = list(n = 0, prop=0))

model_dist %>%
        filter(variable=="model") %>%
        print(n=1000)


model_dist %>%
ggplot() + 
                geom_col(aes(x=response, y=prop, fill=variable), position="dodge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                facet_wrap(~model_name + country, ncol=3)

                           