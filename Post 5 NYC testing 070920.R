library(tidyverse)
library(lubridate)
library(ggrepel)
library(readxl)
library(httr)
library(magrittr)
library(RColorBrewer)

#Grab all the datasets
tests_cases_nyc <- read_csv("https://github.com/nychealth/coronavirus-data/raw/master/tests-by-zcta.csv") %>%
  rename(cases = Positive, tests = Total, pos_rate = modzcta_cum_perc_pos) %>%
  mutate(pos_rate = cases / tests, tests_per_case = tests / cases) 

cases_deaths_nyc <- read_csv("https://github.com/nychealth/coronavirus-data/raw/master/data-by-modzcta.csv") %>%
  rename(modzcta = MODIFIED_ZCTA)

nyc_zips_race <- read_csv("/Users/geoff/Dropbox/CoronaSocial/Data/Local/NYC/Downloaded data/Census Zip Race Info/race_by_zip.csv") %>%
  rename(zcta = NAME) %>% mutate(zcta = as.numeric(str_replace(zcta, "ZCTA5 ", "")))

zip_to_modzcta <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv") %>%
  rename_all(tolower)


#Join and transform as needed for analyses

nyc_zips_covid <- left_join(tests_cases_nyc, cases_deaths_nyc, by = "modzcta")

nyc_modzips_race <- left_join(zip_to_modzcta, nyc_zips_race, by = "zcta") %>%
  mutate(pop = as.numeric(DP05_0033E), black = as.numeric(DP05_0065E), 
         white = as.numeric(DP05_0037E), hisplat = as.numeric(DP05_0071E),
         only_white = as.numeric(DP05_0077E))

nyc_modzips_short <- nyc_modzips_race %>% select(zcta, modzcta, pop, black, white, hisplat, only_white)

nms2 <- nyc_modzips_short %>% group_by(modzcta) %>% 
  mutate(pop = sum(pop), black = sum(black), white = sum(white), 
         hisplat = sum(hisplat), only_white = sum(only_white))

nms3 <- nms2 %>% select(modzcta, pop, white, black, hisplat, only_white) %>% distinct()

nyc_zips <- left_join(nyc_zips_covid, nms3, by = "modzcta")

nyc_zips <- nyc_zips %>% mutate(pct_black = black / pop,
                                pct_white = white / pop,
                                pct_hisplat = hisplat / pop,
                                pct_onlywhite = only_white / pop,)

nyc_zips <- nyc_zips %>% mutate(deaths = COVID_DEATH_COUNT, cases = COVID_CASE_COUNT,
                                newpop = POP_DENOMINATOR, death_rate = deaths / cases,
                                case_rate = cases / newpop, test_rate = tests / newpop,
                                deaths_pop = deaths / newpop, pop_per_case = 1 / case_rate)

nyc_zips <- nyc_zips[!is.na(nyc_zips$modzcta), ]

# Simplified dataframes for certain analyses
b <- nyc_zips %>% filter(pct_black >= .5) 
w <- nyc_zips %>% filter(pct_white >= .5) 
ow <- nyc_zips %>% filter(pct_onlywhite >= .5) 
hl <- nyc_zips %>% filter(pct_hisplat >= .5) 
nb <- nyc_zips %>% filter(pct_black < .5) 

###1: FACT-CHECKING THE HEADLINES ON DEATH RATE IN NYC BY RACE

#1A PER CAPITA DEATHS
#Does a neighborhood's racial/ethnic makeup predict COVID-19 deaths per capita?

lm1a1 <- lm(deaths_pop ~ pct_black, data = nyc_zips)
summary(lm1a1)

lm1a2 <- lm(deaths_pop ~ pct_hisplat, data = nyc_zips)
summary(lm1a2)

lm1a3 <- lm(deaths_pop ~ pct_white, data = nyc_zips)
summary(lm1a3)

lm1a4 <- lm(deaths_pop ~ pct_onlywhite, data = nyc_zips)
summary(lm1a4)

nyc_zips %<>% mutate(pop_per_death = (1 / deaths_pop))

nyc_zips %>% filter(pct_white >= .5 & pop_per_death > 1000) %>%
  count()
nyc_zips %>% filter(pct_white >= .5) %>%
  count()

nyc_zips %>% filter(pct_black >= .5 & pop_per_death > 1000) %>%
  count()
nyc_zips %>% filter(pct_black >= .5) %>%
  count()

#1B ACTUAL DEATH RATE

#Do these neighborhood demographics predict case fatality rate?
lm1b1 <- lm(death_rate ~ pct_black, data = nyc_zips)
summary(lm1b1)

lm1b2 <- lm(death_rate ~ pct_hisplat, data = nyc_zips)
summary(lm1b2)

lm1b3 <- lm(death_rate ~ pct_white, data = nyc_zips)
summary(lm1b3)

lm1b4 <- lm(death_rate ~ only_white, data = nyc_zips)
summary(lm1b4)

#1C DISCREPANCIES IN PREVALENCE
lm2c1 <- lm(case_rate ~ pct_black, data = nyc_zips)
summary(lm2c1)

lm2c2 <- lm(case_rate ~ pct_hisplat, data = nyc_zips)
summary(lm2c2)

lm2c3 <- lm(case_rate ~ pct_white, data = nyc_zips)
summary(lm2c3)

lm2c4 <- lm(case_rate ~ pct_onlywhite, data = nyc_zips)
summary(lm2c4)

#1D ONE LAST CHECK
#Does racial makeup of a neighborhood predict deaths per capita beyond increased prevalence?
#No, even if you analyze that every which way.
lm1d1 <- lm(deaths_pop ~ case_rate * pct_black, data = nyc_zips)
summary(lm1d1)

lm1d2 <- lm(deaths_pop ~ case_rate * pct_white, data = nyc_zips)
summary(lm1d2)

lm1d3 <- lm(deaths_pop ~ case_rate * pct_hisplat, data = nyc_zips)
summary(lm1d3)

lm1d4 <- lm(deaths_pop ~ case_rate * pct_onlywhite, data = nyc_zips)
summary(lm1d4)

lm1d3 <- lm(deaths_pop ~ case_rate * pct_black, weight = newpop, data = nyc_zips)
summary(lm1d3)

lm1d4 <- lm(deaths_pop ~ case_rate * pct_white, weight = newpop, data = nyc_zips)
summary(lm1d4)

mw <- mean(ow$case_rate * 1000)
mb <- mean(b$case_rate * 1000)
mb / mw

#Summary data and Figure 1

nyc_zips %>% filter(pct_white >= .5 & case_rate < .01) %>%
  count()
nyc_zips %>% filter(pct_white < .5 & case_rate < .01) %>%
  count()

nyc_zips %>% filter(pct_white >= .5 & pop_per_case > 60) %>%
  count()

nyc_zips %>% filter(pct_black >= .5 & pop_per_case > 60) %>%
  count()

#Fig 1
nyc_zips %>% ggplot(mapping = aes(pct_black * 100, (pop_per_case), size = 1)) +
  geom_point(color = 'magenta', alpha = .5) +
  geom_smooth(method = loess, size = 1) +
  xlab("% residents identifying as Black or African American") +
  ylab("Residents per infection") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Neighborhood infection rates by race")

ggsave("p5f1.png", width = 8, height = 8, units = "in", dpi = 300)

bq <- nyc_zips %>% filter(pct_black >= .259)
mean(bb$pop_per_case)

## TESTING

mean(nyc_zips$tests_per_case)
mean(b$tests_per_case)
mean(hl$tests_per_case)
mean(w$tests_per_case)
mean(ow$tests_per_case)

##Figure 2
nyc_zips %>% ggplot(mapping = aes(pct_onlywhite * 100, tests_per_case, size = pop)) +
  geom_point(alpha = .5, color = "magenta") +
  geom_smooth(method = lm) + 
  xlab("% residents identifying as White, not Hispanic or Latino") +
  ylab("Tests per confirmed infection") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Testing rates by neighborhood whiteness")

ggsave("p5f2.png", width = 8, height = 8, units = "in", dpi = 300)

mean(nyc_zips$pos_rate)

mean(b$pos_rate)
mean(w$pos_rate)
mean(ow$pos_rate)
mean(hl$pos_rate)
mean(nb$pos_rate)

#Subsequent text

#Is undertesting worse in more black and Hispanic, less-white neighborhoods? Yes.
lm2a <- lm(pos_rate ~ pct_black, data = nyc_zips)
summary(lm2a)

lm2b <- lm(pos_rate ~ pct_hisplat, data = nyc_zips)
summary(lm2b)

lm2a <- lm(pos_rate ~ pct_white, data = nyc_zips)
summary(lm2a)

lm2a <- lm(pos_rate ~ pct_onlywhite, data = nyc_zips)
summary(lm2a)

#Might this be due to a corelation btw racial composition and pop size?
#Perhaps partially, should be tested.
cor(nyc_zips$pct_black, nyc_zips$newpop)
cor(nyc_zips$pct_hisplat, nyc_zips$newpop)
cor(nyc_zips$pct_onlywhite, nyc_zips$newpop)
cor(nyc_zips$pct_white, nyc_zips$newpop)

#Is this solely because of that? No.
lm2c <- lm(pos_rate ~ pct_black * pop, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_onlywhite * pop, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_white * pop, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_onlywhite * pop, data = nyc_zips)
summary(lm2c)



#Is the undertesting at least in part because there are more cases in those neighborhoods? Yes.
lm2b <- lm(case_rate ~ pct_black, data = nyc_zips)
summary(lm2b)

lm2b <- lm(case_rate ~ pct_hisplat, data = nyc_zips)
summary(lm2b)

lm2b <- lm(case_rate ~ pct_white, data = nyc_zips)
summary(lm2b)

lm2w <- lm(case_rate ~ pct_onlywhite, weight = newpop, data = nyc_zips)
summary(lm2w)

#Is this solely because of that? No, there are both main and interaction effects by race.
lm2c <- lm(pos_rate ~ pct_black * case_rate, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_white * case_rate, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_onlywhite * case_rate, data = nyc_zips)
summary(lm2c)

lm2c <- lm(pos_rate ~ pct_onlywhite * case_rate * pop, data = nyc_zips)
summary(lm2c)

##Explanation

lm2c <- lm(tests_per_case ~ case_rate, data = nyc_zips)
summary(lm2c)

lm2c <- lm(tests_per_case ~ pct_onlywhite * case_rate, data = nyc_zips)
summary(lm2c)


#FIGURE 3
nyc_zips <- nyc_zips %>% mutate(majw = ntile(pct_onlywhite, 2))

nyc_zips %>% filter(test_rate < .3) %>%
  ggplot(mapping = aes(case_rate * 100, test_rate * 100, 
                       size = pop, color = pct_onlywhite)) +
  scale_color_continuous(low="#330000", high= "cornsilk") +
  geom_point()  +
  theme(legend.title = element_blank()) +
  geom_smooth(method = lm) +
  xlab("% residents with confirmed COVID-19") +
  ylab("% residents tested") +
  ylim(5, 15) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Test availability by disease prevalence, and race")

ggsave("p5f3.png", width = 8, height = 8, units = "in", dpi = 300)

#Gut-check that this is true in absolute terms, not just when both factors
#are divided by capita

nyc_zips %>% filter(test_rate < .3) %>%
  ggplot(mapping = aes(cases * 100, tests * 100, 
                       size = pop, color = pct_onlywhite)) +
  scale_color_continuous(low="#330000", high= "cornsilk") +
  geom_point()  +
  theme(legend.title = element_blank()) +
  geom_smooth(aes(group = majw), method = lm, se = FALSE, color = '#0066FF') +
  xlab("% residents with confirmed COVID-19") +
  ylab("% residents tested") +
  theme_bw()

ggsave("p5fsupplemental.png", width = 8, height = 8, units = "in", dpi = 300)

#Textual analysis of figure
lm3a <- lm(tests ~ cases * pop, data = nyc_zips)
summary(lm3a)

lm3a <- lm(tests ~ cases * pop * pct_onlywhite, data = nyc_zips)
summary(lm3a)







