
library(tidyverse)
mt_data <- read.csv("data/raw/occurrence/Pk_merged_uncoded_SEA.csv")

fs_data <- read.csv("data/clean/occurrence/pk_present/MBS_FS_B_2005-2014.csv")


fs_data$Source <- "FS"
mt_data$Source <- "MT"

all_data_by_year <- bind_rows(mt_data %>%distinct(ID, .keep_all = TRUE),
                              fs_data %>%distinct(Unique_ID, .keep_all = TRUE)) %>%
  group_by(Source,Year) %>%
  summarise(n_pubs = length(unique(Source_primary)),
            n_data = n())

all_data_by_year$Source <- factor(all_data_by_year$Source, c("MT","FS"))

ggplot(all_data_by_year) +
  geom_col(aes(x=Year, y = n_data, fill = Source), position='stack') +
  scale_fill_manual(values = c("FS" = "#98bfd9", "MT" = "#c7e9c0"),
                    labels = c("FS" = "Prior database (Shearer et al.)",
                               "MT" =  "Current study")) +
  xlab("Year of study start") + ylab("Sample count") +
  scale_x_continuous(breaks = seq(0,10000,by=2),
                     minor_breaks = 1:10000) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = 'gray90'),
        panel.grid.minor.x = element_line(colour = 'gray95'),
        axis.ticks.x = element_line(color = 'gray80')) +
  expand_limits(x = 2020)

ggsave("output/figures/data_epicurve.pdf",
       width = 6, height=4)
