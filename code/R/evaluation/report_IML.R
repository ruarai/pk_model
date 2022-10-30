
require(tidyverse)

iml_files <- list.files(
  "output/update/model_stats/final_1/",
  pattern = "IML",
  full.names = TRUE
)

iml_data <- map(iml_files, read_rds)


iml_importance  <- map_dfr(
  iml_data,
  function(x) {
    map_dfr(x, function(y) y$importance)
  }
) %>% as_tibble() %>%
  filter(!(feature %in% c("Year_Constrained", "wt"))) %>%
  
  group_by(feature) %>%
  mutate(mean_i = mean(importance)) %>%
  ungroup() %>%
  arrange(mean_i) %>%
  mutate(feature = factor(feature, levels = unique(feature)))


ggplot(iml_importance) +
  geom_jitter(aes(y = feature, x = importance),
              size = 0.5, alpha = 0.5) +
  
  
  theme_minimal()



iml_ale <- map_dfr(
  iml_data,
  function(x) {
    map_dfr(x, function(y) y$ale, .id = "ix_2")
  },
  .id = "ix_1"
) %>% as_tibble() %>%
  mutate(sample = as.numeric(ix_1) * as.numeric(ix_2))


plot_ale <- map_dfr(
  unique(iml_ale$var),
  function(i_var) {
    iml_ale %>%
      filter(var == i_var) %>%
      
      group_by(sample) %>%
      
      summarise(interp_x = seq(min(.$x), max(.$x), length.out = 100),
                y = approxfun(x, .value)(interp_x),
                
                .groups = "drop") %>%
      
      rename(x = interp_x) %>%
      ungroup() %>%
      
      group_by(x) %>%
      summarise(mean = mean(y, na.rm = TRUE),
                u_95 = quantile(y, probs = 0.975, na.rm = TRUE),
                l_95 = quantile(y, probs = 0.025, na.rm = TRUE),
                u_50 = quantile(y, probs = 0.75, na.rm = TRUE),
                l_50 = quantile(y, probs = 0.25, na.rm = TRUE),
                
                .groups = "drop") %>%
      
      mutate(var = i_var)
  }
)
  
plot_ale_sorted <- plot_ale %>%
  filter(var != "Host_species") %>%
  
  mutate(var = factor(var, levels = unique(rel_inf$var)) %>%
           fct_rev())

ggplot(plot_ale_sorted) +
  
  geom_hline(yintercept = 0, color = 'grey30') +
  
  geom_ribbon(aes(x = x, ymin = l_95, ymax = u_95),
              alpha = 0.5, fill = '#55843b') +
  
  geom_ribbon(aes(x = x, ymin = l_50, ymax = u_50),
              alpha = 0.5, fill = '#55843b') +
  
  geom_line(aes(x = x, y = mean),
            color = '#55843b') +
  
  xlab("Value of predictor") + ylab("Mean effect on prediction") +
  
  scale_x_continuous(breaks = scales::breaks_extended(),
                     labels = scales::label_comma(),
                     guide = guide_axis(check.overlap = TRUE)) +
  
  facet_wrap(~var, scales = "free_x") +
  
  theme_minimal()

ggsave(
  "output/figures/ALE_feature_effect.png",
  width = 12,
  height = 9,
  bg = "white"
)


model_files <- list.files(
  "output/update/bootstrap_outputs/final_1",
  full.names = TRUE
)

# rel_inf <- map_dfr(
#   model_files,
#   function(i_file) {
#     map_dfr(read_rds(i_file), function(x) x$relinf)
#   }
# )

rel_inf <- read_csv("output/update/model_stats/final_1/relinf.csv") %>%
  group_by(var) %>%
  mutate(mean_ri = mean(rel.inf),
         u95_ri = quantile(rel.inf, probs = 0.975),
         l95_ri = quantile(rel.inf, probs = 0.025)) %>%
  ungroup() %>%
  arrange(mean_ri) %>%
  mutate(var = factor(var, levels = unique(var)))

round2 <- function(x) format(round(x, 2), nsmall = 2) %>% str_trim()

ggplot(rel_inf, aes(y = var, x = rel.inf)) +
  
  stat_summary(fun = mean,
               fun.max = function(x) quantile(x, probs = 0.975),
               fun.min = function(x) quantile(x, probs = 0.025),
               
               pch = '-',
               
               size = 1, stroke = 1,
               
               color = '#951d66') +
  coord_cartesian(xlim = c(0,60)) +
  
  scale_x_continuous(breaks = scales::breaks_extended(7)) +
  
  
  stat_summary(fun = mean,
               
               size = 0.4, stroke = 1,
               
               color = '#951d66') +
  
  theme_minimal() +
  
  xlab("Relative influence") + ylab(NULL)


ggsave(
  "output/figures/relative_influence.png",
  width = 7,
  height = 6,
  bg = "white"
)
