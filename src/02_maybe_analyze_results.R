library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(ggridges)
library(tidyverse)

trimean <- function(x){
        qs <- quantile(x, probs = c(0.25, 0.5, 0.75))
        
        trimean <- (qs[1] + 2*qs[2] + qs[3]) / 4
        names(trimean) <- NULL
        
        return(trimean)
}



# ---------------------------------------------------------------------------
# read_quizas_json()
#
# Reads the JSON produced by quizas_simulation.py and returns a tidy tibble.
# Uses fromJSON() + tidyr unnesting for vectorised performance — avoids
# row-by-row purrr loops over the full record set.
#
# Output schema (one row per expression / pair per profile):
#
#   profile_id       chr   e.g. "profile_1005_gemma3:4b"
#   model            chr   e.g. "gemma3:4b"
#   Country_label    chr
#   Age              int
#   Sex_label        chr
#   Education_label  chr
#   Class_label      chr
#   Occupation_label chr
#   section          chr   "A" | "B" | "C"
#   expression       chr   label of the expression or pair
#   value            dbl   point value (A, C) or midpoint (B)
#   min              dbl   NA for A and C; range min for B
#   max              dbl   NA for A and C; range max for B
# ---------------------------------------------------------------------------

read_quizas_json <- function(path) {
        
        # fromJSON with simplifyDataFrame=FALSE keeps the nested list structure
        # intact, which is what we need before we flatten each section ourselves.
        raw <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
        
        n <- length(raw)
        profile_ids <- names(raw)
        
        # ── 1. Demographics ───────────────────────────────────────────────────────
        # Extract profile_variables for all records at once with vapply / lapply.
        pv_list <- lapply(raw, `[[`, "profile_variables")
        
        demo <- tibble(
                profile_id       = profile_ids,
                model            = str_extract(profile_ids, "[^_]+$"),
                Country_label    = vapply(pv_list, function(x) x$Country_label    %||% NA_character_, character(1)),
                Age              = as.integer(vapply(pv_list, function(x) x$Age   %||% NA_integer_,  numeric(1))),
                Sex_label        = vapply(pv_list, function(x) x$Sex_label        %||% NA_character_, character(1)),
                Education_label  = vapply(pv_list, function(x) x$Education_label  %||% NA_character_, character(1)),
                Class_label      = vapply(pv_list, function(x) x$Class_label      %||% NA_character_, character(1)),
                Occupation_label = vapply(pv_list, function(x) x$Occupation_label %||% NA_character_, character(1)),
        )
        
        # ── 2. Survey responses ───────────────────────────────────────────────────
        sr_list <- lapply(raw, `[[`, "survey_response")
        
        # Helper: flatten one named list of scalars into a two-column data.frame
        flatten_scalar <- function(lst, sec_label) {
                if (is.null(lst) || !is.list(lst)) {
                        return(data.frame(section = sec_label, expression = NA_character_,
                                          value = NA_real_, min = NA_real_, max = NA_real_,
                                          stringsAsFactors = FALSE))
                }
                data.frame(
                        section    = sec_label,
                        expression = names(lst),
                        value      = as.numeric(unlist(lst)),
                        min        = NA_real_,
                        max        = NA_real_,
                        stringsAsFactors = FALSE
                )
        }
        
        # Helper: flatten section B (list of {min, max}) into a three-column data.frame
        flatten_range <- function(lst) {
                if (is.null(lst) || !is.list(lst)) {
                        return(data.frame(section = "B", expression = NA_character_,
                                          value = NA_real_, min = NA_real_, max = NA_real_,
                                          stringsAsFactors = FALSE))
                }
                lo <- as.numeric(vapply(lst, `[[`, numeric(1), "min"))
                hi <- as.numeric(vapply(lst, `[[`, numeric(1), "max"))
                data.frame(
                        section    = "B",
                        expression = names(lst),
                        value      = (lo + hi) / 2,
                        min        = lo,
                        max        = hi,
                        stringsAsFactors = FALSE
                )
        }
        
        # Build a list of per-profile long data.frames, then rbind all at once.
        # This is much faster than growing a tibble row by row.
        long_list <- vector("list", n)
        
        for (i in seq_len(n)) {
                sr <- sr_list[[i]]
                if (!is.list(sr)) {
                        long_list[[i]] <- data.frame(
                                section = NA_character_, expression = NA_character_,
                                value = NA_real_, min = NA_real_, max = NA_real_,
                                stringsAsFactors = FALSE
                        )
                } else {
                        long_list[[i]] <- rbind(
                                flatten_scalar(sr$section_a, "A"),
                                flatten_range(sr$section_b),
                                flatten_scalar(sr$section_c, "C")
                        )
                }
        }
        
        # Repeat each demo row to match its section row count, then column-bind
        row_counts <- vapply(long_list, nrow, integer(1))
        demo_expanded <- demo[rep(seq_len(n), times = row_counts), ]
        row.names(demo_expanded) <- NULL
        
        bind_cols(demo_expanded, do.call(rbind, long_list)) |> as_tibble()
}


# ---------------------------------------------------------------------------
# read_quizas_many()
#
# Iterates read_quizas_json() over a character vector of file paths and
# returns a single combined tibble. Adds a `source_file` column so rows
# can be traced back to their origin file.
#
# Arguments:
#   paths      chr vector of JSON file paths
#   .progress  logical; print a progress message per file (default TRUE)
# ---------------------------------------------------------------------------

read_quizas_many <- function(paths, .progress = TRUE) {
        
        results <- vector("list", length(paths))
        
        for (i in seq_along(paths)) {
                if (.progress) message(sprintf("[%d/%d] Reading %s", i, length(paths), paths[[i]]))
                df <- read_quizas_json(paths[[i]])
                results[[i]] <- dplyr::mutate(df, source_file = basename(paths[[i]]), .before = 1)
        }
        
        dplyr::bind_rows(results)
}


# Multiple files:
paths <- list.files("./data/quizas_quizas_quizas/resps/", pattern = "\\.json$", full.names = TRUE)
df    <- read_quizas_many(paths)

df <- df %>%
        mutate(expression = str_remove(expression, "- "))

section_a <- df %>%
        filter(section == "A")  %>%
        mutate(expression = fct_rev(factor(expression, 
                                   levels = c("Seguro", "Casi seguro", "Seguramente", "Muy probable",
                                              "Pienso que sí", "Creo que sí", 
                                              "Probable", "Puede ser", "Quizás", "No sé",
                                              "Igual al azar", "Lo dudo", "Poco probable",
                                              "Difícilmente", "Improbable", "Muy improbable"))))


section_a %>%
        ggplot(aes(x=value, y=expression, group=expression, fill = factor(after_stat(quantile)))) + 
        stat_density_ridges(quantiles = c(0.25,0.5,0.75)
                            , quantile_lines = TRUE
                            , geom = "density_ridges_gradient"
                            , alpha = 0.6
                            , scale = 2.3) + 
        scale_fill_viridis_d(
                           , name = "Quantile"
                           , alpha = 0.3
                           , option = "cividis") +
        facet_wrap(~model) + 
        theme_minimal()

section_a %>%
        group_by(model, expression) %>%
        reframe(q1 = quantile(value, probs=0.25),
                median = median(value),
                q3 = quantile(value, probs=0.75),
                trimean = trimean(value)
                IQR = IQR(value),
                mean = mean(value),
                sd = sd(value)
                ) %>%
        mutate(cvar = sd/mean*100)


#
# Quick checks:
#   df |> count(source_file, section)
#   df |> filter(section == "A") |> group_by(expression) |>
#          summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
#   df |> filter(section == "B") |> select(profile_id, expression, min, max, value)