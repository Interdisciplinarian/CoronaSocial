library(tidyverse)
library(lubridate)
library(ggrepel)
library(readxl)
library(httr)
library(olsrr)

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

#Load and prepare cultural dimensions data and merge with coronavirus data

GET("https://geerthofstede.com/wp-content/uploads/2016/08/6-dimensions-for-website-2015-08-16.xls",
    config = httr::config(ssl_verifypeer = 0L),
    na = c("", "#NULL!"),
    write_disk(cd_tf <- tempfile(fileext = ".xls")))

cultural_dimensions <- read_xls(cd_tf) %>%
  mutate(country = str_replace(country, "Bosnia and Herzegovina", "Bosnia")) %>%
  mutate(country = str_replace(country, "Czech Rep", "Czechia")) %>%
  mutate(country = str_replace(country, "Dominican Rep", "Dominican Republic")) %>%
  mutate(country = str_replace(country, "Great Britain", "United Kingdom")) %>%
  mutate(country = str_replace(country, "Korea South", "South Korea")) %>%
  mutate(country = str_replace(country, "Kyrgyz Rep", "Kyrgyzstan")) %>%
  mutate(country = str_replace(country, "Slovak Rep", "Slovakia")) %>%
  mutate(country = str_replace(country, "Trinidad_and Tobago", "Trinidad and Tobago")) %>%
  mutate(country = str_replace(country, "U.S.A.", "United States"))

corona_social_global <- left_join(cases_deaths_global, cultural_dimensions, by = "country")
csg_summary <- cases_deaths_global %>% group_by(country) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), 
    pop = mean(pop2018, na.rm = TRUE)) %>% 
  mutate(cases_100k = (cases / pop) * 100000, 
    deaths_100k = (deaths / pop) * 100000, death_rate = deaths / cases)

#Create complete and summary of COVID-19 cases, deaths, and culture by country
corona_social_global <- left_join(cases_deaths_global, cultural_dimensions, by = "country")

csg_summary <- cases_deaths_global %>% group_by(country) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), 
            pop = mean(pop2018, na.rm = TRUE)) %>% 
  mutate(cases_100k = (cases / pop) * 100000, 
         deaths_100k = (deaths / pop) * 100000, death_rate = deaths / cases)
csg_summary <- left_join(csg_summary, cultural_dimensions, by = "country")

sum(!is.na(csg_summary$idv)&!is.na(csg_summary$pdi))

# pdi graph with log-linear model
ggplot(csg_summary,
       aes(pdi,log(cases_100k),
           colour=idv,size=pop)) +
  geom_point()+
  geom_label_repel(data=filter(csg_summary,pop>2E8| pdi < 25 | pdi> 100),
                   aes(pdi,log(cases_100k),
                       label=country), inherit.aes = F)+
  geom_smooth(method='lm',formula=y~x,se=F,colour='black',lty=2)+
  scale_colour_continuous(type='viridis')+
  scale_y_continuous(
    breaks=as.vector(log(outer(c(1,2,5),10^seq(-1,3)))),
    labels=function(v)sprintf("1 in %1.0f", 100000 * exp(-v)),
    limits=log(range(10^seq(-1,3)))) +
  guides(size = F) +
  labs(
    title = 'COVID-19 Case Prevalence by Country',
    x = 'Power Distance',
    colour='Individualism',
    y = 'Confirmed Cases'
  )

ggsave("p3_pdi.png", height=5, width=6, dpi=100)

# Individualism
ggplot(csg_summary,
       aes(idv,log(cases_100k),
           colour=pdi,size=pop)) +
  geom_point()+
  geom_label_repel(data=filter(csg_summary,pop>2E8| pdi < 25 | pdi> 100),
                   aes(idv,log(cases_100k),
                       label=country), inherit.aes = F)+
  geom_smooth(method='lm',formula=y~x,se=F,colour='black',lty=2)+
  scale_colour_continuous(type='viridis',trans='reverse')+
  scale_y_continuous(
    breaks=as.vector(log(outer(c(1,2,5),10^seq(-1,3)))),
    labels=function(v)sprintf("1 in %1.0f", 100000 * exp(-v)),
    limits=log(range(10^seq(-1,3))))+
  guides(size = F)+
  labs(
    title = 'COVID-19 Case Prevalence by Country',
    x = 'Individualism',
    colour='Power Distance',
    y = 'Confirmed Cases'
  )
ggsave("p3_idv.png", height=5, width=6, dpi=100)

# a bootstrap confidence interval on correlation between pdi and idv among 62 countries
nboot <- 1000
corvec <- rep(0,nboot)
for(i in 1:nboot){
  csg_summary %>% 
    filter(!is.na(pdi)) %>% 
    sample_frac(1, replace=T) %>% 
    select(pdi,idv) %>% 
    cor -> cormat
  corvec[i] <- cormat[1,2]
}
# 95% confidence interval on correlation (bootstrap)
quantile(corvec, c(.05,.95))

# full cultural dimension (cd) correlation matrix 
for_matrix <- csg_summary[c("freedom_score", "pop", "pdi", "idv", "mas", "uai", "ltowvs", "ivr")]
cd_correlations <- cor(for_matrix, use = "complete.obs")
cd_correlations

#Figure for post

csg_summary <- mutate(csg_summary, pdidv = (pdi + (100- idv)))

conf <- .95
cases_freedom_lm2 <- lm(log(cases_100k) ~ pdidv, 
                        data = csg_summary,
                        weights = sqrt(pop))
summary(cases_freedom_lm2)
tibble(pdidv = seq(0, 200, 1), country = "", pop = 0.) %>% 
  bind_cols(as_tibble(predict(cases_freedom_lm2, ., 
                              interval = "confidence",
                              level = conf))) %>% 
  gather(kind, logcases_100k, 
         -c(pdidv, country, pop)) %>% 
  mutate(cases_100k = exp(logcases_100k)) %>% 
  select(-logcases_100k) %>% 
  bind_rows(csg_summary %>% 
              mutate(kind = 'actual') %>% 
              select(pdidv, country, pop, kind, cases_100k)) %>% 
  mutate(kind=recode(kind, fit="MLE", lwr="Lower Confidence Bound", 
                     upr="Upper Confidence Bound")) -> plotdata1

ggplot(mapping = aes(pdidv, log(cases_100k))) +
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
    mapping = aes(x = pdidv,
                  ymax = log(`Upper Confidence Bound`),
                  ymin = log(`Lower Confidence Bound`)),
    alpha = .25,
    colour = 'gray',
    inherit.aes = F
  ) +
  geom_label_repel(
    data = plotdata1 %>% 
      filter(kind == "actual",
             pop > 1E8 | (pop > 1E7 & pdidv < 100)),
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
    title = "COVID-19 rate by power distance and collectivism",
    y = "Confirmed cases per resident",
    x = "Power distance and collectivism",
    linetype = "",
    colour = "",
    size="Population"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

ggsave("p3.png", width = 8, height = 8, units = "in", dpi = 300)


#PDI*IDV regression, PDIDV regression to comapre

cases_pdi_idv_lm_wt <- lm(log(cases_100k) ~ pdi + idv, weights = pop, data = csg_summary)
summary(cases_pdi_idv_lm_wt)

cases_pdidv_lm_wt <- lm(log(cases_100k) ~ pdidv, weights = pop, data = csg_summary)
summary(cases_pdidv_lm_wt)

# ratio and common language effect size 

x1 <- csg_summary[!is.na(csg_summary$pdi),]
cases1 <- with(x1, {cases_100k[pdi <= quantile(pdi, .25)]})
cases2 <- with(x1, {cases_100k[pdi >  quantile(pdi, .75)]})

#ratio
ratio <- mean(cases1) / mean(cases2)
ratio


# compute the probability that a country in bottom quartile pdi 
# has more cases than a country in the top quartile pdi.
country_pairs <- data.frame(
  cases1 = rep(cases1, times = length(cases2)),
  cases2 = rep(cases2, each = length(cases1))
)

country_pairs$event_2_gt_1 <- with(country_pairs, {cases2 > cases1})
prob_2_gt_1 <- mean(country_pairs$event_2_gt_1)



