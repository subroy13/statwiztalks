library(wordcloud)
library(wordcloud2)

acts <- c("Civil Rights Act (1964)", 
          "Education Amendments (1972)",
          "Equal Credit Opportunity Act",
          "Fair Housing Act", 
          "Equal Pay Act (1963)", 
          "Immigration Reform and Control Act", 
          "Age Discrimination in Employment Act (1967)",
          "Pregnency Discrimination Act",
          "Rehabilitation Act (1973)",
          "Genetic Information Nondiscrimination Act",
          "Uniformed Services Employment Rights Act",
          "Vietnam Era Veterans' Readjustment Assistance Act")

wordcloud(acts, freq = 12:1, min.freq = 1, 
          max.words = 100, scale = c(1.5, 0.5), rot.per = 0, 
          colors = brewer.pal(8, "Dark2"))


###############################################3
setwd("D:/Academic/Projects/2021_PCM_Presentation")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


theme_538 <- function(..., base_size = 12) {
    
    theme(
        # plotting components
        
        ## drop minor gridlines
        panel.grid.minor = element_blank(),
        # change grid lines to gray
        panel.grid.major =  element_line(color = "#d0d0d0"),
        # fill the plot and panel spaces with grey and remove border
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        plot.background = element_rect(fill = "#f0f0f0", color = NA),
        panel.border = element_blank(),
        # remove strip background
        strip.background = element_blank(),
        # adjust the margins of plots and remove axis ticks
        plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
        axis.ticks = element_blank(),
        # change text family, size, and adjust position of titles
        text = element_text(family = "Chivo", size = base_size),
        axis.text = element_text(face = "bold", color = "grey", size = base_size),
        axis.title = element_text(face = "bold", size = rel(1.33)),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
        plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
        plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
        strip.text = element_text(size = rel(1.33), face = "bold"),
        ...
    )
}

# COMPAS Score
df <- read_csv('./datasets/compas-scores-two-years.csv')

tmpdf <- df %>%
    group_by(race, decile_score) %>%
    summarise(count = n()) %>%
    ungroup() %>% 
    add_row(tibble(race = unique(tmpdf$race), decile_score = 0, count = 0)) %>%
    group_by(race) %>%
    arrange(decile_score) %>%
    mutate(ecdf = cumsum(count) / sum(count))

ggplot(tmpdf) +
    geom_line(aes(x = decile_score, y = ecdf, color = race), size = 1) +
    xlab("COMPAS Score") + ylab("CDF by Group") +
    guides(color = guide_legend(title = "Race")) +
    theme_538()


###########################
tmpdf2 <- df %>%
    filter(race %in% c("African-American", "Caucasian")) %>%
    group_by(race, decile_score, two_year_recid) %>%
    summarise(count = n()) %>%
    ungroup() %>% 
    add_row(tibble(race = c("African-American", "Caucasian", "African-American", "Caucasian"), two_year_recid = c(0,0,1,1), decile_score = 0, count = 0)) %>%
    group_by(race, two_year_recid) %>%
    arrange(decile_score) %>%
    mutate(ecdf = cumsum(count) / sum(count))

ggplot(tmpdf2) +
    geom_line(aes(x = decile_score, y = ecdf, color = race, linetype = factor(two_year_recid)), size = 1) +
    xlab("COMPAS Score") + ylab("CDF by (Race, Recidivism)") +
    guides(color = guide_legend(title = "Race"), linetype = guide_legend(title = "Recidivism")) +
    theme_538()

# Calculating the ROC curves
tmpdf3 <- df %>% 
    filter(race %in% c("African-American", "Caucasian")) %>%
    group_by(race, decile_score) %>%
    summarise(count = n(), recidcount = sum(two_year_recid)) %>%
    ungroup() %>%
    group_by(race) %>%
    arrange(decile_score) %>%
    mutate(
        tp = rev(cumsum(rev(recidcount))),
        fp = rev(cumsum(rev(count - recidcount))),
        fn = c(0, cumsum(recidcount)[-10]),
        tn = c(0, cumsum(count - recidcount)[-10]),
        tpr = tp / (tp + fn),
        fpr = fp / (fp + tn)
    )


ggplot(tmpdf3) +
    geom_line(aes(x = fpr, y = tpr, color = race), size = 1) +
    xlab("False Positive Rate") + ylab("True Positive Rate") +
    guides(color = guide_legend(title = "Race")) +
    theme_538() +
    coord_cartesian(xlim = c(0.4, 0.5), ylim = c(0.6, 0.8))








