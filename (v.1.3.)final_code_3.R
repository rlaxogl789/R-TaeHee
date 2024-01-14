library(tidyverse)
library(tidycensus)
library(sf)
library(ggmap)
library(ipumsr)
library(RColorBrewer)
library(ggplot2)

# base data
setwd("C:/Users/82108/Desktop/연세대 수업/2023-2 수업/부동산경제데이터/urban_data/data")
data <- read_rds("final_data.rds")
data <- data %>% filter(STATEFIP != 2 & STATEFIP != 15)
state <- ipumsr::ipums_val_labels(data$STATEFIP)
job <- ipumsr::ipums_val_labels(data$OCC)

data_a <- data %>% filter(STATEFIP == 4)
data_n <- data %>% filter(STATEFIP == 35)

usa <- get_acs(geography = "state", variables = c(pop="B01003_001", medhh="B19013_001"), 
               output="wide", year = 2019, geometry=TRUE)
usa <- usa  %>% filter(!NAME %in% c("Hawaii", "Alaska", "Puerto Rico")) %>% 
  st_simplify(dTolerance = 100) %>% arrange(GEOID)

usa


# house price

data_val <- data %>%
  filter(VALUEH < 9999998) %>%
  group_by(YEAR, STATEFIP) %>%
  summarise(value = mean(VALUEH)) %>%
  left_join(state, by=c("STATEFIP"="val"))

data_vd <- data_val %>% 
  group_by(lbl) %>% 
  summarise(vd = nth(value,2) / nth(value,1))

usa_vd <- usa %>% left_join(data_vd, by = c("NAME" = "lbl")) # NAME 이랑 lbl 일치되도록 오른쪽에 column 붙임 
view(usa)
view(usa_vd)
usa_vd$group <- cut_interval(usa_vd$vd, 5)

ggplot() +
  geom_sf(aes(fill=group), usa_vd) +  # sf 는 simple feature. geometry 정보 담고 있는 데이터 
  scale_fill_brewer(palette = "YlOrRd")+ 
  theme_void() + 
  labs(title="House Price : 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))



# population

data_pop <- data %>%
  group_by(YEAR, STATEFIP) %>% 
  summarise(pop = sum(PERWT)) %>% 
  left_join(state, by=c("STATEFIP"="val"))

data_popd <- data_pop %>% 
  group_by(lbl) %>% 
  summarise(popd = nth(pop,2) / nth(pop,1))

data_popd %>%
  ggplot(aes(x = popd, y = reorder(lbl, popd))) + 
  geom_point()+ 
  labs(title="Population difference : 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))

data_popd %>%
  ggplot(aes(x = popd, y = reorder(lbl, popd), color = popd)) + 
  geom_point() + 
  scale_color_viridis_c() +  # Using a viridis color scale
  labs(title = "Population difference: 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))



# income

data_inc <- data %>% 
  filter(INCTOT < 9999999) %>% 
  group_by(YEAR, STATEFIP) %>%
  summarise(income = weighted.mean(INCTOT, PERWT)) %>%
  left_join(state, by=c("STATEFIP"="val"))

data_incd <- data_inc %>% 
  group_by(lbl) %>% 
  summarise(incd = nth(income,2) / nth(income,1))

data_incd %>%
  ggplot(aes(x = incd, y = reorder(lbl, incd))) + 
  geom_point()+ 
  labs(title="Income change : 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))



# poverty 

data_pov <- data %>%
  filter(POVERTY > 0) %>% 
  group_by(YEAR, STATEFIP) %>%
  summarise(poverty = weighted.mean(POVERTY, PERWT)) %>%
  left_join(state, by=c("STATEFIP"="val"))

data_povd <- data_pov %>% 
  group_by(lbl) %>% 
  summarise(povd = nth(poverty,2) / nth(poverty,1))

usa_povd <- usa %>% left_join(data_povd, by = c("NAME" = "lbl"))

usa_povd$group <- cut_interval(usa_povd$povd, 5)

ggplot() +
  geom_sf(aes(fill=group), usa_povd) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme_void() + 
  labs(title="Poverty Rate Ratio : 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))


#employ

data_emp <- data %>% 
  mutate(emp = if_else(EMPSTAT == 1, 1, 0)) %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(ep = weighted.mean(emp, PERWT)) %>% 
  left_join(state, by=c("STATEFIP"="val"))

data_empd <- data_emp %>% 
  group_by(lbl) %>% 
  summarise(empd = nth(ep,2) / nth(ep,1))

usa_empd <- usa %>% left_join(data_empd, by = c("NAME" = "lbl"))

usa_empd$group <- cut_interval(usa_empd$empd, 5)

ggplot() +
  geom_sf(aes(fill=group), usa_empd) + 
  scale_fill_brewer(palette = "YlOrRd")+ 
  theme_void() + 
  labs(title="Employment Ratio : 2019 Vs 2021") + 
  theme(plot.title = element_text(hjust = 0.5))



#OCC

occ_a <- data_a %>% 
  filter(YEAR == 2019, OCC != 0) %>% 
  group_by(OCC) %>% 
  summarise(pop_ad = sum(PERWT) / 7278717 * 100) %>% 
  arrange(desc(pop_ad)) %>% 
  top_n(15)

occ_n <- data_n %>% 
  filter(YEAR == 2019, OCC != 0) %>% 
  group_by(OCC) %>% 
  summarise(pop_nd = sum(PERWT) / 2096829 * 100) %>% 
  arrange(desc(pop_nd)) %>% 
  top_n(15)

occ_and <- left_join(occ_a, occ_n, by = c("OCC")) %>% 
  mutate(pop_d = pop_ad - pop_nd, job = as.character(OCC)) %>% 
  arrange(pop_d)


ggplot(occ_and) +
  geom_bar(aes(x = reorder(job, pop_d), y = pop_d, fill = pop_d), stat = 'identity') + 
  scale_fill_viridis_c() +  # Using a viridis color scale
  labs(
    title = "Change of Population Ratio for Occupation: Arizona - New Mexico",
    caption = "5240: Customer service representatives, 4020: Cooks"
  ) +
  ylab("Population Difference (%)") +
  scale_x_discrete(name = "Occupation Category")

# https://usa.ipums.org/usa/volii/occ2018.shtml (occ description )
# Arizona customer service representatives increased alot 
# New Mexico physical therapist, cooks increased a lot 


# OCC diff

data_occdiff <- data %>% 
  filter(OCC == 5240 | OCC == 4020) %>% 
  mutate(year = as.character(YEAR), job = as.character(OCC))


ggplot(data_occdiff) +
  geom_bar(aes(year, fill = job, weight = PERWT), position = "dodge") + 
  scale_y_continuous(name="Population",
                     labels=scales::label_comma(scale=0.001)) +
  scale_x_discrete(name="Year") +
  scale_fill_discrete(name="Job",
                      labels=c("Cooks", "Customer Service")) +
  labs(title = "Change of population for Jobs : Arizona - New Mexico", caption = "population unit : 1000") + 
  theme(legend.position = "bottom")




