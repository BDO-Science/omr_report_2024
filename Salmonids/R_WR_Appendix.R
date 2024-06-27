
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(here)


report_year = 2024

# Escapement -------------------------------------

## Download data
url_escapement <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
escapement <- read_csv(url_escapement) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4)),
         Year = factor(Year2)) %>%
  filter(Year2 > report_year -11)


## Make plot
(plot_escapement <- ggplot(escapement) + 
  geom_col(aes(Year, Annual), fill = "steelblue4") +
  geom_hline(yintercept = mean(escapement$Annual), linetype = "dashed") + 
  labs(y = "Escapement", x = "Brood Year")+
  theme_bw() +
    theme(axis.text = element_text(size = 11),
          axis.title = element_text(size = 12)))

## Write plot
tiff("Salmonids/output/Figure_escapement.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_escapement
dev.off()



# JPI ------------------------------
jpi <- read.csv(here("Salmonids/data/JPI_2002_2023.csv")) %>%
  filter(BY <= report_year) %>%
  rename(JPI = Fry.Equivalent.JPI,
         ETF_Survival = ETF.Survival.Rate....) %>%
  select(BY, JPI, ETF_Survival) %>%
  mutate(JPI = JPI/1000000,
         JPI_lab = round(JPI, 2),
         ETF_Survival_lab = round(ETF_Survival)) %>%
  filter(BY > report_year - 11) %>%
  mutate(BY = factor(BY)) 

(plot_jpi <- ggplot(jpi) + 
    geom_col(aes(BY, JPI), fill = "#C69214", alpha = 0.8, width = 0.8) +
    geom_text(aes(BY, JPI+0.15, label = JPI_lab), size = 4.5) + 
    geom_hline(yintercept = mean(jpi$JPI), linetype = "dashed") + 
    labs(y = "Juvenile Production Index (millions)") +
    scale_y_continuous() + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.title.x = element_blank()))

## Write plot
tiff("Salmonids/output/Figure_jpi.tiff", width = 8, height = 6, units = "in", res = 300, compression = "lzw")
plot_jpi
dev.off()

# TDM and ETF --------------------------------------

tdm <- read.csv(here("Salmonids/data/ETF_TDM_2002_2023.csv")) %>%
  mutate(unexplained_mortality = 100-ETF_Survival-TDM_NOAA_percent) %>%
  rename(ETF_survival = ETF_Survival) %>%
  filter(Brood.Year > report_year-11) %>%
  mutate(Brood.Year = factor(Brood.Year))%>%
  mutate(color = case_when(Sac.Val.Year.Type == "C" ~ "#D55E00",
                           Sac.Val.Year.Type == "D" ~ "#E69F00",
                           Sac.Val.Year.Type == "AN" ~ "#009E73",
                           Sac.Val.Year.Type == "BN" ~  "black",
                           Sac.Val.Year.Type == "W" ~ "#0072B2")) %>%
  mutate(Brood.Year.Type = paste0(Brood.Year, " (", Sac.Val.Year.Type, ")" )) 

tdm_long <- tdm %>%
  select(Brood.Year.Type, color,
         `Temperature Attributed Mortality` = TDM_NOAA_percent, 
         `Egg-to-Fry Survival` = ETF_survival, 
         `Unattributed Mortality` = unexplained_mortality) %>%
  pivot_longer(cols = `Temperature Attributed Mortality`:`Unattributed Mortality`, names_to = "Fate", values_to = "Percent") %>%
  mutate(Percent_label = round(Percent)) 

yrcolors <- rev(tdm$color)

(plot_tdm <- ggplot(tdm_long, aes(Brood.Year.Type, Percent, fill = Fate)) + 
    geom_col(width = 0.65, alpha = 0.9) +
    geom_text(aes(label = Percent_label), position = position_stack(vjust = 0.5), size = 5) +
    scale_fill_manual(values = c("goldenrod","steelblue" ,"gray70")) + 
    labs(x = "Brood Year") + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12, colour = yrcolors),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          legend.position = "bottom",
          legend.title = element_blank()))

(plot_etf <- ggplot(jpi) + 
    geom_col(aes(BY, ETF_Survival), fill = "#007396", alpha = 0.8, width = 0.8) +
    geom_text(aes(BY, ETF_Survival +1, label = ETF_Survival_lab), size =  4) + 
    geom_hline(yintercept = mean(jpi$ETF_Survival), linetype = "dashed") + 
    labs(y = "Egg-to-Fry Survival (%)") +
    # scale_y_continuous(expand = c(0,0)) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.title.x = element_blank()))

## Write plot
tiff("Salmonids/output/Figure_tdm.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_tdm
dev.off()

tiff("Salmonids/output/Figure_etf.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
plot_etf
dev.off()

# Hatchery
