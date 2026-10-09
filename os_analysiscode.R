############################################################
##                                                        ##
##              Observational Funders' study              ##
##                                                        ##
############################################################

# To reproduce, set your working directory first!
setwd(".......................")

# Library
library(readxl)
library(tidyr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyverse)
library(gt)
library(stringr)
library(webshot2)

# Calling the data
os <- read_xlsx("os_grant_publication.xlsx")
View(os)

# Note: open science in the grants are marked with "g_", g means grants.
# Open science in publications marked with "s_" . S means study.
# Code 1 = open science practice done
# Code 0 = open science practice not done


## Data cleaning
os <- os |>
        rename('g_availability_protocol' = `availability protocol`,
               `g_study_registration` = `study registration`,
               `g_open_materials` = `open materials`,
               `g_open_data` = `open data`,
               `g_open_code` = `open code`,
               `g_code_availability` = `code availability`,
               `g_software_tools` = `software tools`,
               `g_citation_to_materials` = `citation to materials`,
               `g_citation_to_data` = `citation to data`,
               `g_citation_to_code` = `citation to code`,
               `g_transparent_reporting` = `transparent reporting`,
               `g_open_access` = `open access`,
               `g_preprint` = `preprint`,
               `g_registered_report` = `registered report`,
               `s_registered_report` = `s_register report`
        )

os <- os |>
        rename_with(~ gsub(" ", "_", .x)) 


# Recoding field based on frascati ----------------------------------------

os <- os |>
        mutate(
                frascati_code = case_when(
                        field %in% c(
                                "medicine", "biomedicine", "epidemiology",
                                "public health", "immunology"
                        ) ~ 3,
                        
                        field %in% c(
                                "bioengineering", "biotechnology"
                        ) ~ 2,
                        
                        field %in% c(
                                "animal science", "forest science"
                        ) ~ 4,
                        
                        field %in% c(
                                "psychology", "urban study",
                                "anthropology", "economics"
                        ) ~ 5,
                        
                        field == "geography" ~ 1,
                        
                        TRUE ~ 1
                ),
                
                frascati_field = recode(
                        as.character(frascati_code),
                        `1` = "Natural sciences",
                        `2` = "Engineering and technology",
                        `3` = "Medical and health sciences",
                        `4` = "Agricultural and veterinary sciences",
                        `5` = "Social sciences"
                )
        )

os |>
        count(frascati_code, frascati_field) |>
        arrange(frascati_code)

tg_field <- os |>
        tbl_summary(
                include = frascati_field,
                label = list(frascati_field ~ "Research Field (Frascati standardised category)")
        )

tg_field

gt_field <- as_gt(tg_field)
gtsave(gt_field, "tg_field.png")


# Funders location --------------------------------------------------------

# Count publications by funding source
funding_data <- os |>
        filter(!is.na(country)) |>
        count(country, name = "n") |>
        mutate(
                funding_source = reorder(country, n)
        )

# Horizontal dot plot
tg_source <- ggplot(funding_data,
       aes(x = n, y = funding_source)) +
        geom_segment(
                aes(x = 0, xend = n,
                    yend = funding_source),
                colour = "grey75",
                linewidth = 0.6
        ) +
        geom_point(size = 3) +
        labs(
                x = "Number of matched pair grant proposals-publications",
                y = "Funding source (country/region)"
        ) +
        scale_x_continuous(
                breaks = seq(0, max(funding_data$n) + 5, by = 10),
                expand = expansion(mult = c(0, 0.05))
        ) +
        theme_classic(base_size = 12) +
        theme(
                axis.text = element_text(colour = "black"),
                axis.title = element_text(colour = "black")
        )

tg_source

ggsave(
        filename = "tg_location.png",
        plot = tg_source,
        width = 8,
        height = 6,
        dpi = 300
)


# Funded years ------------------------------------------------------------

# Count funded grant proposals per year
year_data <- os |>
        filter(!is.na(year)) |>
        count(year, name = "n") |>
        arrange(year)

# A lollipop chart
p_year <- ggplot(year_data, aes(x = factor(year), y = n)) +
        geom_segment(
                aes(xend = factor(year), y = 0, yend = n),
                colour = "grey65",
                linewidth = 0.7
        ) +
        geom_point(
                size = 3.5,
                colour = "#2878A5"
        ) +
        labs(
                x = "Year of grant funding",
                y = "Number of funded grant proposals"
        ) +
        theme_classic(base_size = 12) +
        theme(
                axis.text = element_text(colour = "black"),
                axis.title = element_text(colour = "black")
        )

p_year

ggsave(
        filename = "tg_year.png",
        plot = p_year,
        width = 8,
        height = 5,
        units = "in",
        dpi = 300
)

# Comparison Open science in the grants vs publications -------------------

practice_order <- gsub("^g_|^s_", "", 
                       colnames(os)[grepl("^g_", colnames(os))])

practice_order_clean <- practice_order |>
        str_replace_all("_", " ") |>
        str_to_title()

plot_data <- os |>
        pivot_longer(
                cols = starts_with("g_") | starts_with("s_"),
                names_to = c("source", "practice"),
                names_pattern = "^(g|s)_(.*)$",
                values_to = "value"
        ) |>
        group_by(source, practice) |>
        summarise(
                count = sum(value == 1, na.rm = TRUE),
                percent = mean(value == 1, na.rm = TRUE) * 100,
                .groups = "drop"
        ) |>
        mutate(
                source = recode(source,
                                g = "Proposals",
                                s = "Publications"),
                practice = str_replace_all(practice, "_", " "),
                practice = str_to_title(practice),
                practice = factor(practice,
                                  levels = practice_order_clean)
        )

bar_os <- ggplot(plot_data,
                 aes(x = practice,
                     y = count,
                     fill = source)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(aes(label = count),
                  position = position_dodge(width = 0.9),
                  vjust = -0.3,size = 3) +
        labs(title = "Open Science Practices: Grant proposals vs Publications",
             x = "Open Science Practice",
             y = "Cumulative",
             fill = "Source") +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 11)
        ) +
        expand_limits(y = max(plot_data$count) + 10)

bar_os

ggsave("bar_os.png", plot = bar_os, 
       width = 13, height = 6, dpi = 500, bg = "white")

# Matrix transition -------------------------------------------------------

# Note: Transition heat map (0→0, 0→1, 1→0, 1→1)
# This explain where the increase comes from grant --> publication

transition <- os |>
        pivot_longer(
                cols = study_id,
                names_to = c("stage", "practice"),
                names_pattern = "^(g|s)_(.*)$",
                values_to = "value"
        )

colnames(transition)

long_os <- os |>
        pivot_longer(
                cols = c(starts_with("g_"), starts_with("s_")),
                names_to = c("stage", "practice"),
                names_pattern = "^(g|s)_(.*)$",
                values_to = "value"
        ) |>
        pivot_wider(
                names_from = stage,
                values_from = value
        ) |>
        mutate(
                transition = case_when(
                        g == 0 & s == 0 ~ "0→0",
                        g == 0 & s == 1 ~ "0→1",
                        g == 1 & s == 0 ~ "1→0",
                        g == 1 & s == 1 ~ "1→1"
                )
        )

transition_summary <- long_os |>
        group_by(practice, transition) |>
        summarise(n = n(), .groups = "drop") |>
        group_by(practice) |>
        mutate(percent = n / sum(n) * 100, 1)


# Heat map

practice_order <- gsub("^g_|^s_", "", 
                       colnames(os)[grepl("^g_", colnames(os))])

transition_summary$practice <- factor(
        transition_summary$practice,
        levels = practice_order
)

m <- ggplot(transition_summary,
            aes(x = transition, y = practice, fill = percent)) +
        geom_tile(color = "white") +
        scale_y_discrete(labels = function(x) {
                x <- gsub("_", " ", x)
                tools::toTitleCase(x)
        }) +
        scale_fill_gradient(low = "white", high = "plum4") +
        labs(
                x = "Transition Type",
                y = "Open Science Practice",
                fill = "%"
        ) +
        theme_minimal()+
        theme(
                axis.text.y = element_text(size = 12),
                axis.title.y = element_text(size = 14),
                axis.text.x = element_text(size = 12),
                axis.title.x = element_text(size = 14),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 12)
        )

m

ggsave("transition_matrix.png", plot = m, 
       width = 12, height = 6, dpi = 500, bg = "white")


# Comparison of consistent practices --------------------------------------

agreement_table <- long_os |>
        group_by(practice) |>
        summarise(
                total = n(),
                agreement_n = sum(g == s, na.rm = TRUE),
                realised_n  = sum(g == 1 & s == 1, na.rm = TRUE),
                .groups = "drop"
        ) |>
        mutate(
                agreement_pct = agreement_n / total * 100,
                realised_pct  = realised_n / total * 100
        )

# order
practice_order <- gsub("^g_|^s_", "", 
                       colnames(os)[grepl("^g_", colnames(os))])

agreement_table$practice <- factor(
        agreement_table$practice,
        levels = practice_order
)

table_pub <- agreement_table |>
        arrange(practice) |>   
        mutate(
                practice = str_replace_all(as.character(practice), "_", " "),
                practice = str_to_title(practice),
                
                `Agreement n (%)` = sprintf("%d (%.1f%%)", agreement_n, agreement_pct),
                `Realised n (%)`  = sprintf("%d (%.1f%%)", realised_n, realised_pct)
        ) |>
        select(
                Practice = practice,
                `Agreement n (%)`,
                `Realised n (%)`
        )

table_gt <- table_pub |>
        gt() |>
        tab_header(
                title = "Alignment of Open Science Practices: Proposals vs Publications"
        ) |>
        cols_align(
                align = "center",
                columns = -Practice
        ) |>
        tab_source_note(
                source_note = 
                        "Total sample size = 101. Agreement defined as identical values (0–0 or 1–1). Realised defined as 1–1."
        )

table_gt

gtsave(table_gt, "alignment_os_table.png")


# This is the END of the analysis -----------------------------------------

# Author: Ayu Dewi
# Website: https://ayudewi.github.io/Portfolio/
# This code is free to reuse without permission.
