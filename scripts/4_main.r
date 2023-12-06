

library(tidyverse)
library(fixest)
library(patchwork)
library(ragg)


# run the two scripts below for data:
# source("scripts/clean.r")
# source("scripts/regobjects.r")


# Regressions table -------------------------------------------------------------

# standaradizing trust measure and adding countries
left_join(contdata, whoregions) %>% 
  mutate(trust = (trust-mean(trust, na.rm = T))/sd(trust, na.rm = T)) %>% 
  mutate(jointcome = (relcome + subcome)/2) -> tempdata

#running fixed effects regressions (using feols from fixest)
modelsummary::modelsummary(
  models = list(
    "relative" = feols(data = tempdata, 
                       trust~sex+age+edu+emp+round+relcome|country, se = "cluster"),
    "subjective" = feols(data = tempdata, 
                         trust~sex+age+edu+emp+round+subcome|country, se = "cluster"),
    "both" = feols(data = tempdata, 
                   trust~sex+age+edu+emp+round+relcome+subcome|country, se = "cluster")
  ),
  fmt = 2,
  stars = T,
  output = "flextable"
) %>% 
  
  flextable::save_as_docx(path = "figuresntables/regtable.docx")

# countries by region -------------------------------------------------------------------

#making temporary objects
left_join(cregs, whoregions) %>%
  mutate(
    term = str_replace(term, "subcome", "Subjective income"),
    term = str_replace(term, "relcome", "Relative income"),
    region = str_replace(region, " M", "\nM")
  ) %>% 
  left_join(merged %>% group_by(country) %>% summarise(vtrust = sd(trust, na.rm = T))) %>% 
  mutate(
    estimate = estimate/vtrust, std.error = std.error/vtrust, 
    country = paste(country, term),
    country = fct_reorder(country, estimate, max)
  ) -> temp_country

rregs %>% 
  left_join(left_join(merged, whoregions) %>% group_by(region) %>% summarise(vtrust = sd(trust, na.rm = T))) %>% 
  mutate(
    term = str_replace(term, "subcome", "Subjective income"),
    term = str_replace(term, "relcome", "Relative income"),
    estimate = estimate/vtrust, std.error = std.error/vtrust,
    region = str_replace(region, " M", "\nM")
  ) -> temp_region


#the subjective pane
ggplot(
  data = temp_country %>% filter(term == "Subjective income"),
  aes(
    y = estimate, 
    ymin = estimate-1.96*std.error, ymax = estimate+1.96*std.error,
    group = term
  )
) +
  geom_pointrange(aes(x = country), size = .2, color = aucolr::picker(c("red"))) +
  geom_hline(yintercept = 0) +
  geom_hline(
    data = temp_region %>% filter(term == "Subjective income"),
    aes(yintercept = estimate),
    linetype = "dashed"
  ) +
  geom_rect(
    data = temp_region %>% filter(term == "Subjective income"),
    aes(xmin = 0, xmax = Inf),
    alpha = .2
  ) +
  ylab("Subjective\nIncome") +
#The relative pane
ggplot(
    data = temp_country %>% filter(term == "Relative income"),
    aes(
      y = estimate, 
      ymin = estimate-1.96*std.error, ymax = estimate+1.96*std.error,
      group = term
    )
  ) +
  geom_pointrange(aes(x = country), size = .2, color = aucolr::picker()) +
  geom_hline(yintercept = 0) +
  geom_hline(
    data = temp_region %>% filter(term == "Relative income"),
    aes(yintercept = estimate),
    linetype = "dashed"
  ) +
  geom_rect(
    data = temp_region %>% filter(term == "Relative income"),
    aes(xmin = 0, xmax = Inf),
    alpha = .2
  ) +
  ylab("Relative\nIncome") +
  
#arguements applid to both
  plot_layout(ncol = 1, guides = "collect") &
  jtools::theme_nice() &
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.x = element_text(angle = 90, hjust = 0, size = 9)
  ) &
  scale_y_continuous(breaks = c(-.25,0,.25,.5)) &
  xlab("") &
  facet_grid(.~region, scales = "free_x", space='free_x')


ggsave("figuresntables/fig1.tiff", width = 24, height = 16, units = "cm", dpi = 600)

# map ---------------------------------------------------------------------

left_join(cregs, whoregions) %>% 
  left_join(merged %>% group_by(country) %>% summarise(vtrust = sd(trust, na.rm = T))) %>% 
  mutate(est = estimate/vtrust, se = std.error/vtrust, country = fct_reorder(country, est, max)) %>%
  
  left_join(
    map_data("world") %>% 
      rename(country = region) %>% 
      mutate(country = case_when(
        country == "USA" ~ "United States",
        country == "UK" ~ "United Kingdom",
        country == "Palestine" ~ "Palestinian Territories",
        country == "Democratic Republic of the Congo" ~ "Congo",
        country == "Swaziland" ~ "Eswatini",
        country == "Gambia" ~ "The Gambia",
        subregion == "Hong Kong" ~ "Hong Kong",
        T ~ country
      ))
  ) %>%
  
  mutate(
    term = case_when(
      term == "relcome" ~ "Relative Income",
      term == "subcome" ~ "Subjective Income"
    ),
    term = fct_rev(term)
  ) %>% 
  
  ggplot(aes(long, lat, group = group, fill = est)) +
  geom_polygon(color = "black") +
  coord_quickmap() +
  ylab("") +
  xlab("") +
  jtools::theme_nice(legend.pos = "bottom") +
  scale_fill_gradient2(low = aucolr::picker("red"), mid = "grey", high = aucolr::picker()) +
  
  facet_wrap(term~., ncol = 1) +
  
  theme(
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  ) 

ggsave("figuresntables/fig2.tiff", width = 24, height = 16, units = "cm", dpi = 600)



# cross level interactions ------------------------------------------------

#load in in regressions
#load(file = "objects/lv2regs.rdata")

bind_rows(
  broom.mixed::tidy(lv2aggrrel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2spenrel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2gdprel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2ginirel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2demrel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2mortrel) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2aggrsub) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2spensub) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2gdpsub) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2ginisub) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2demsub) %>% slice(11:13) %>% mutate(model = last(term)),
  broom.mixed::tidy(lv2mortsub) %>% slice(11:13) %>% mutate(model = last(term))
) %>% 
  
  separate(model, c("outcome", "moderator"), ":") %>% 
  
  mutate(stars = case_when(
    p.value <.001 ~ "***",
    p.value <.01 ~ "**",
    p.value <.05 ~ "*",
    .default = ""
  )) %>% 
  transmute(
    outcome,
    moderator,
    term = str_replace(term, ".*:.*", "interaction"),
    coefficient = paste0(round(estimate, 2), 
                         " (", round(std.error, 2), ")",
                         stars
    )
  ) %>% 
  mutate(
    term = str_replace(term, "relcome", "income"),
    term = str_replace(term, "subcome", "income")
  ) %>% 
  
  pivot_wider(id_cols = c(moderator, term), names_from = outcome, values_from = coefficient) %>% 
  mutate(moderator = case_when(moderator == lag(moderator) ~ "", .default = moderator)) %>% 
  modelsummary::datasummary_df(output = "flextable") %>% 
  
  flextable::save_as_docx(path = "figuresntables/crosslevelinteractions.docx")


# by country factors -----------------------------------------------------

bind_rows(
  cregs18 %>% mutate(round = "2018"), 
  cregs20 %>% mutate(round = "2020")
) %>% 
left_join(lv2) %>% 
  mutate(estimate = estimate/vtrust) %>% 
  mutate(
    term = str_replace(term, "relcome", "Relative income"),
    term = str_replace(term, "subcome", "Subjective income"),
    term = fct_rev(term)
  ) %>% 
  pivot_longer(10:19) %>% 
  rename(var = name) -> tempdata
  

tempdata %>% filter(var == "aggtrust") %>% 
  ggplot(aes(y = estimate, x = value, color = term)) +
  xlab("Country trust") + ylab("Regression coefficient") +
  jtools::theme_nice(legend.pos = "topleft") +
  guides(shape = "none") +
  theme(
    legend.title = element_blank(), 
    legend.direction = "vertical",
    legend.spacing.y = unit(0, 'cm')
  ) +
  ggtitle("Culture of science") +
  
  tempdata %>% filter(var == "gdppc") %>% 
  ggplot(aes(y = estimate, x = value, color = term)) +
  xlab("GDP PC, in 1000s") + ylab("") +
  scale_x_continuous(trans = "sqrt", breaks = c(25000,50000,75000,100000), label = scales::label_number(scale = .001)) +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.y = element_blank()) +
  ggtitle("Economic situation") +
  
  tempdata %>% filter(var == "v2x_polyarchy") %>% 
  ggplot(aes(y = estimate, x = value, color = term)) +
  xlab("Democracy") + ylab("") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.y = element_blank()) +
  ggtitle("Institutional quality") +
  
  tempdata %>% filter(var == "scispend") %>% 
  ggplot(aes(y = estimate, x = value, color = term)) +
  xlab("Science Spending") + ylab("Regression coefficient") +
  jtools::theme_nice(legend.pos = "none") +
  
  tempdata %>% filter(var == "gini") %>% 
  ggplot(aes(y = estimate, x = value, color = term), axis.text.y = element_blank()) +
  xlab("GINI") + ylab("") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.y = element_blank()) +
  
  tempdata %>% filter(var == "infmort") %>% 
  ggplot(aes(y = estimate, x = value, color = term)) +
  xlab("Infant mortality") + ylab("") +
  scale_x_continuous(trans = "sqrt") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.y = element_blank()) +
  
  
  plot_layout(nrow = 2) &
  geom_hline(yintercept = 0, color = "gray60") &
  geom_point(aes(shape = round), alpha = .1) &
  geom_smooth(se = F, alpha = .8) &
  stat_smooth(geom = "line", method = "lm", linetype = "dashed", alpha = .5, linewidth = .5) &
  scale_color_manual(values = aucolr::picker(c("red", "blue"))) &
  coord_cartesian(ylim = c(-.25,.75)) &
  theme(plot.title = element_text(hjust = 0.5))

ggsave("figuresntables/fig3.tiff", width = 24, height = 16, units = "cm", dpi = 600)

# individual level interaction --------------------------------------------

indiinterz %>% 
  mutate(
    est = estimate/sd(contdata$trust, na.rm = T),
    se = std.error/sd(contdata$trust, na.rm = T),
    term = case_when(
      term == "relcome" ~ "Relative\nIncome",
      term == "subcome" ~ "Subjective\nIncome"
    ),
    term = fct_rev(term),
    var = fct_inorder(var),
    value = fct_inorder(value),
    value = fct_relevel(value, "Not at all")
  ) %>% 
  
  ggplot(
    aes(y = est, x = value, color = term, ymin = est-1.96*se, ymax = est+1.96*se)
  ) +
  geom_pointrange(position = position_dodge(.3)) +
  geom_hline(yintercept = 0) +
  
  xlab("") +
  ylab("Regression coefficient") +
  facet_grid(.~var, scales = "free_x", space = "free_x") +
  jtools::theme_nice(legend.pos = "top") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.spacing.x = unit(.6, "cm"),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = aucolr::picker(c("red", "blue")))


ggsave("figuresntables/fig4.tiff", width = 24, height = 16, units = "cm", dpi = 600)




