library(tidyverse)
library(lubridate)
library(ggrepel)
library(readxl)
library(httr)

#Load, rename, and create per-capita numbers with European CDC data
cases_deaths_global <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
  na = c("", "NaN")) %>% 
  rename(country = countriesAndTerritories, date = dateRep, 
    country_code = countryterritoryCode, pop2018 = popData2018) %>%
  mutate(country = str_replace(country, "United_States_of_America", "United States")) %>%
  mutate(date = dmy(date)) %>% mutate(cases_100k = (cases / pop2018) * 100000,
    deaths_100k = (deaths / pop2018) * 100000,  death_rate = deaths / cases, na.rm = TRUE) %>%
  group_by(country) %>% mutate(cases_cum = cumsum(cases), deaths_cum = cumsum(deaths)) %>%
  mutate(cases_cum_100k = (cases_cum / pop2018) * 100000, 
    deaths_cum_100k = (deaths_cum / pop2018) * 100000, death_rate_cum = deaths_cum / cases_cum)

#Load and prrepare Freedom House data and merge with coronavirus data
GET("https://freedomhouse.org/sites/default/files/2020-02/2020_All_Data_FIW_2013-2020.xlsx",
    write_disk(fh_tf <- tempfile(fileext = ".xlsx")))

freedom_house <- read_xlsx(fh_tf, 2L, skip = 1) %>% filter(Edition == 2020) %>%
  rename(country = `Country/Territory`, freedom_score = Total, status = Status)

corona_social_global <- left_join(cases_deaths_global, freedom_house, by = "country") 

csg_summary <- cases_deaths_global %>% group_by(country) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), 
    pop = mean(pop2018, na.rm = TRUE)) %>% 
  mutate(cases_100k = (cases / pop) * 100000, 
    deaths_100k = (deaths / pop) * 100000, death_rate = deaths / cases)

csg_summary <- left_join(csg_summary, freedom_house, by = "country") 

#Figure 1 and discussion
csg_summary %>% ggplot(mapping = aes(freedom_score, cases_100k)) +
  geom_point() +
  ylim(-10, 1000) +
  geom_smooth(method = lm, se = FALSE)+
  labs(
    y = "Cases per 100,000 people",
    x = "Freedom Score",
    title = "   COVID-19 prevalence by
    Freedom House rating"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("p2f1.png", width = 4, height = 4, units = "in", dpi = 500)

cases_freedom_lm <- lm(cases_100k ~ freedom_score, data = csg_summary)
summary(cases_freedom_lm)

csg_summary %>% filter(freedom_score <= 50, cases_100k >= 200) %>% 
  arrange(desc(pop)) 
csg_summary %>% filter(freedom_score > 50, cases_100k >= 200) %>% 
  arrange(desc(pop))

#Figure 2 and discussion
csg_summary %>% 
  ggplot(mapping = aes(freedom_score, cases_100k)) +
  geom_point(
    mapping = aes(size=pop),
    alpha = .5,
    color = "red",
    show.legend = FALSE
  ) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_label(
    data = csg_summary %>% filter(
    country %in% c("Russia", "Spain", "United States", "China", 
                   "Italy", "India", "Canada", "Japan",
                   "Nigeria", "Brazil", "Iraq", "Mexico",
                   "Turkey", "Iran")),
    mapping = aes(label = country),
    vjust=-.3, alpha=.5
  ) +
  ylim (0, 520) +
  theme_classic() +
  labs(
    title = "COVID-19 prevalence by Freedom House rating",
    y = "Cases per 100,000 people",
    x = "Freedom Score",
    size="Population"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("p2f2.png", width = 8, height = 8, units = "in", dpi = 300)

cases_freedom_lm_wt <- lm(cases_100k ~ freedom_score, weights = pop, data = csg_summary)
summary(cases_freedom_lm_wt)

#Figure 3 and discussion

conf <- .95
cases_freedom_lm2 <- lm(log(cases_100k) ~ freedom_score, 
                       data = csg_summary,
                       weights = sqrt(pop))
summary(cases_freedom_lm2)
tibble(freedom_score = seq(0, 100, 1), country = "", pop = 0.) %>% 
  bind_cols(as_tibble(predict(cases_freedom_lm2, ., 
                              interval = "confidence",
                              level = conf))) %>% 
  gather(kind, logcases_100k, 
         -c(freedom_score, country, pop)) %>% 
  mutate(cases_100k = exp(logcases_100k)) %>% 
  select(-logcases_100k) %>% 
  bind_rows(csg_summary %>% 
              mutate(kind = 'actual') %>% 
              select(freedom_score, country, pop, kind, cases_100k)) %>% 
  mutate(kind=recode(kind, fit="MLE", lwr="Lower Confidence Bound", 
                     upr="Upper Confidence Bound")) -> plotdata1

ggplot(mapping = aes(freedom_score, log(cases_100k))) +
  geom_point(
    data = plotdata1 %>% filter(kind == "actual"),
    mapping = aes(size=pop),
    alpha = .5,
    color = "red",
    show.legend = FALSE
  ) +
  geom_line(
    data = plotdata1 %>% filter(kind == "MLE"),
    size = 1.1,
    color = "blue"
  ) +
  geom_ribbon(
    data = 
      plotdata1 %>% 
      filter(kind %in% c("Lower Confidence Bound",
                         "Upper Confidence Bound")) %>% 
      pivot_wider(names_from = kind, values_from = cases_100k),
    mapping = aes(x = freedom_score,
                  ymax = log(`Upper Confidence Bound`),
                  ymin = log(`Lower Confidence Bound`)),
    alpha = .25,
    colour = 'gray',
    inherit.aes = F
  ) +
  geom_label_repel(
    data = plotdata1 %>% 
      filter(kind == "actual",
             pop > 1E7 & cases_100k > quantile(
               cases_100k, .95, na.rm = T) |
               pop > 1E7 & cases_100k < quantile(cases_100k, .05, na.rm = T)
      ),
    mapping = aes(label = str_replace(country, "_", " ")),
    hjust='right', min.segment.length = 0.,
    box.padding = .1,
    size = 3,
    segment.alpha = .5
  ) +
  scale_y_continuous(
    breaks = as.vector(log(outer(c(1, 2, 5),10^seq(-1, 4)))),
    labels = function(v) sprintf("1 in %1.0f", 1E+5 / exp(v)),
    limits = log(c(.1, 2000))
  ) +
  scale_size_continuous(
    breaks = exp(seq(log(1E5), log(1E9), length.out=5))
  ) +
  guides(linetype = F, color = F)+
  theme_minimal()+
  labs(
    title = "COVID-19 rate by Freedom House rating",
    y = "Confirmed cases per resident",
    x = "Freedom Score",
    linetype = "",
    colour = "",
    size="Population"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

ggsave("p2f3.png", width = 8, height = 8, units = "in", dpi = 1000)

# what percent increase in disease rate to expect 
# for 10 more points in freedom score?
(exp(coef(cases_freedom_lm2)[[2]]*10)-1)*100
