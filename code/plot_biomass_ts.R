load(here::here("data/biomass_timeseries_final.Rdata"))


# Create plot ------------------------------------------------------------------
# Create line plot - single
biomass.ts.line <- ggplot(filter(biomass.ts, biomass != 0),
                          aes(x = date_start, y = biomass/10^6,
                              colour = group, shape = group,
                              group = group)) +
  geom_bar(colour = "gray50", fill = "gray50", position = "stack", stat = "identity") +
  geom_errorbar(aes(ymin = biomass_ci_lower/10^6, ymax = biomass_ci_upper/10^6), width = 5000000) +
  geom_path() +
  geom_point(size = 2, fill = "white") +
  scale_colour_manual(name = 'Species',
                      labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                 "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                 "Scomber japonicus", "Trachurus symmetricus"),
                      values = c(pac.herring.color, anchovy.color, "#93F09F",
                                 rnd.herring.color, sardine.color, "#FF7256",
                                 pac.mack.color, jack.mack.color)) +
  scale_shape_manual(name = 'Species',
                     labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                "Scomber japonicus", "Trachurus symmetricus"),
                     values = c(21, 22, 23,
                                21, 22, 23,
                                21, 22)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(Mt)))) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.line,
       filename = here("figs/biomass_ts_line.png"),
       width = 8, height = 4)

# Filter data to include 2021-2023
biomass.ts.sub <- biomass.ts %>%
  filter(year >= 2021)

# Import Carranza data
biomass.ts.jcf <- read_csv("data/biomass_ts_Carranza.csv") %>%
  filter(year >= 2021)


# Create stacked bar plot
biomass.ts.bar <- ggplot(biomass.ts.jcf,
                         aes(x = year, y = biomass/10^6,
                             fill = group, colour = group)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(name = 'Species',
                    labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                               "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color, "#93F09F",
                               rnd.herring.color, sardine.color, "#FF7256",
                               pac.mack.color, jack.mack.color)) +
  scale_colour_manual(name = 'Species',
                    labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                               "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color, "#93F09F",
                               rnd.herring.color, sardine.color, "#FF7256",
                               pac.mack.color, jack.mack.color)) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(expression(Biomass~(italic(Mt)))) +
  ylab(expression(Biomass~(italic(t)))) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0),
        legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.bar,
       filename = here("figs/biomass_ts_bar.png"),
       width = 8, height = 4)

# Combine plots into one using {patchwork}
biomass.ts.combo <- biomass.ts.line/biomass.ts.bar +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# Save figure
ggsave(biomass.ts.combo,
       filename = here("figs/biomass_ts_combo.png"),
       width = 8, height = 8)
