## Erin J. Campbell
## Redlining and Asthma Summary Stats
## July 2021

library(tidyverse)
library(sf)

d <- readRDS("FINAL_default.rds")
a <- readRDS("D:\\Packaged Code\\Table 1\\FINAL_alternate.rds")

a %>% distinct(city)

a %>% group_by(holc_grade) %>%
  summarise(n = n(),
            m_rhi = mean(`Total Respiratory (hazard quotient)`, na.rm = T),
            sd_rhi = sd(`Total Respiratory (hazard quotient)`, na.rm = T),
            m_asth = mean(Data_Value, na.rm = T),
            sd_asth = sd(Data_Value, na.rm = T)) 


a %>% group_by(holc_grade) %>%
  summarise(n = n(),
            m_pop = mean(population, na.rm = T),
            sd_pop = sd(population, na.rm = T),
            m_popdens = mean(pop_density, na.rm = T),
            sd_popdens = sd(pop_density, na.rm = T),
            m_o65 = mean(o65, na.rm = T),
            sd_o65 = sd(o65, na.rm = T),
            m_u18 = mean(u18, na.rm = T),
            sd_u18 = sd(u18, na.rm = T))

a %>% group_by(holc_grade) %>%
  summarise(n = n(),
            m_male = mean(male, na.rm = T),
            sd_male = sd(male, na.rm = T),
            m_white = mean(white, na.rm = T),
            sd_white = sd(white, na.rm = T),
            m_black = mean(black, na.rm = T),
            sd_black = sd(black, na.rm = T),
            m_native = mean(native, na.rm = T),
            sd_native = sd(native, na.rm = T),
            m_asian = mean(asian, na.rm = T),
            sd_asian = sd(asian, na.rm = T),
            m_hispanic = mean(hisp, na.rm = T),
            sd_hispanic = sd(hisp, na.rm = T))



a %>% group_by(holc_grade) %>%
  summarise(m_bach = mean(educ_bachplus, na.rm = T),
            sd_bach = sd(educ_bachplus, na.rm = T),
            m_pov = mean(poverty, na.rm = T),
            sd_pov = sd(poverty, na.rm = T),
            m_mhi = mean(mhi, na.rm = T),
            sd_mhi = sd (mhi, na.rm = T))

a %>% group_by(holc_grade) %>%
  summarise(m_detatched = mean(detatched_unit, na.rm = T),
            sd_detatched = sd(detatched_unit, na.rm = T),
            m_2plus = mean(unit_2plus, na.rm = T),
            sd_2plus = sd(unit_2plus, na.rm = T),
            m_own = mean(owner_occ, na.rm = T),
            sd_own = sd(owner_occ, na.rm = T),
            m_pre1940 = mean(pre1940, na.rm = T),
            sd_pre1940 = sd(pre1940, na.rm = T),
            m_mgr = mean(med_gross_rent, na.rm = T),
            sd_mgr = sd(med_gross_rent, na.rm = T),
            m_mhv = mean(med_value, na.rm = T),
            sd_mhv = sd(med_value, na.rm = T)
            )

cor(d$`Total Respiratory (hazard quotient)`, d$Data_Value, use = "complete.obs")
