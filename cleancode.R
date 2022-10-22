## Erin J. Campbell
## Redlining and Asthma Data Wrangling
## October 2021

# This is the cleaned and condensed code for the RLxAsthma paper's data

library(tidyverse)
library(sf)
library(geojsonsf)
library(tidycensus)
library(lwgeom)
library(tigris)
library(readxl)

## Redlining geojson 
####################
RL<-geojsonsf::geojson_sf("D:\\Packaged Code\\Dataframe Development\\fullDownload.geojson")

# change grade E to grade D
RL$holc_grade[RL$holc_grade=="E"] <- "D"

# prepare SES and Health data characteristics at census tract level 
###################################################################

options(tigris_use_cache = TRUE)
census_api_key("252c5bf28058f94a17530826db8cdc2f16886624", 
               install = TRUE,
               overwrite =  TRUE)

#ACS
vars<-c(race_total = "B02001_001E",
        race_white = "B02001_002E",
        race_black = "B02001_003E",
        race_native = "B02001_004E",
        race_asian = "B02001_005E",
        eth_total = "B03003_001E",
        eth_hisp = "B03003_003E",
        total = "B01001_001E",
        total_male = "B01001_002E",
        male_u5 = "B01001_003E",
        male_5_9 = "B01001_004E",
        male_10_14 = "B01001_005E",
        male_15_17 = "B01001_006E",
        male_65 = "B01001_020E",
        male_67 = "B01001_021E",
        male_70 = "B01001_022E",
        male_75 = "B01001_023E",
        male_80 = "B01001_024E", 
        male_85 = "B01001_025E", 
        total_female = "B01001_026E",
        female_u5 = "B01001_027E",
        female_5_9 = "B01001_028E",
        female_10_14 = "B01001_029E",
        female_15_17 = "B01001_030E",
        female_65 = "B01001_044E",
        female_67 = "B01001_045E",
        female_70 = "B01001_046E",
        female_75 = "B01001_047E",
        female_80 = "B01001_048E",
        female_85 = "B01001_049E", 
        total_h = "B25003_001E",
        owner_occupied_h = "B25003_002E",
        total_u = "B25024_001E",
        detatched_u = "B25024_002E",
        u_2 = "B25024_004E",
        u_3 = "B25024_005E",
        u_5 = "B25024_006E",
        u_10 = "B25024_007E",
        u_20 = "B25024_008E",
        u_50 = "B25024_009E",
        total_structure = "B25034_001E",
        pre_1940 = "B25034_011E",
        med_year_built = "B25034_011E",
        med_gross_rent = "B25064_001E",
        med_value = "B25077_001E",
        mhi = "B06011_001E",
        pov_total = "B16009_001E",
        pov_below = "B16009_002E",
        educ_total = "B07009_001E",
        educ_bach = "B07009_005E",
        educ_higher = "B07009_006E",
        lang_total = "B26113_001E",
        lang_otherthan_eng = "B26113_003E")


ctys <- counties(cb = TRUE)

state_codes <- unique(fips_codes$state_code)[1:51]

#use 2017 to match with the 500 cities database
acs <- map_df(state_codes, function(state_code) {
  state <- filter(ctys, STATEFP == state_code)
  county_codes <- state$COUNTYFP
  get_acs(geography = "tract",
          variables = vars,
          state = state_code, 
          county = county_codes, 
          geometry = T,
          output = "wide",
          key = "252c5bf28058f94a17530826db8cdc2f16886624",
          year = 2017)
})

#DECENNIAL
us<-unique(fips_codes$state)[1:51]
dec<-map_df(us, function(x){
  get_decennial(geography = "tract",
                variables = c(population="P001001"),
                state = x,
                output = "wide",
                year = 2010,
                key = "252c5bf28058f94a17530826db8cdc2f16886624")
})

health <- read_csv("D:\\Packaged Code\\Dataframe Development\\500_Cities__Current_asthma_among_adults_aged___18_years.csv")

#filter health data to city and tract levels separately
health_city <- health %>% filter(GeographicLevel == "City")
health <- health %>% filter(GeographicLevel == "Census Tract") %>%
  dplyr::select(TractFIPS, DataValueTypeID, DataSource, CityName, StateAbbr, Year, Data_Value, PopulationCount)

#bring in rhi data and clean down to tract level estimates
rhi <- read_xlsx("D:\\Packaged Code\\Dataframe Development\\nata.xlsx")
rhi %>% filter(County != "Entire US" & County != "Entire State") %>%
  dplyr::select(State, County, FIPS, Tract, `Total Respiratory (hazard quotient)`) -> rhi

# merging all census tract level data
m1<-merge(acs, dec, by ="GEOID")
m2<-merge(m1, health, by.x = "GEOID", by.y = "TractFIPS")
SES<-merge(m2, rhi, by.x = "GEOID", by.y = "Tract", all.x = TRUE)

# create variables
SES$area<-st_area(SES$geometry)

SES %>% mutate(pop_density = population/area) %>%
  mutate(white = race_white/race_total,
            black = race_black/race_total,
            native = race_native/race_total,
            asian = race_asian/race_total,
            hisp = eth_hisp/eth_total,
            male = total_male/total,
            u18 = (male_u5 + male_5_9 + male_10_14 + male_15_17
                   + female_u5 + female_5_9 + female_10_14 + female_15_17)/total,
            o65 = (male_65 + male_67 + male_70 + male_75 + male_80 + male_85 
                    + female_65 + female_67 + female_70 + female_75 + female_80 + female_85)/total,
            owner_occ = owner_occupied_h/total_h,
            detatched_unit = detatched_u/total_u,
            unit_2plus = (u_2 + u_3 + u_5 + u_10 + u_20 + u_50)/total_u,
            pre1940 = pre_1940/total_structure,
            poverty = pov_below/pov_total,
            educ_bachplus = (educ_bach + educ_higher)/educ_total)->SES1

SES1 %>% dplyr::select(GEOID, NAME.x,
         med_gross_rent, 
         med_value,
         mhi,
         population:educ_bachplus
         )-> SES

# resetting the crs of the census data
SES <- st_transform(SES, 4326)

# Geometric merge to assign historic holc grade
###############################################

RL <- RL %>% mutate(HOLCID = row_number())
SES <- SES %>% mutate(CENSUSTRACTID = row_number())


RL_CT_Intersect<-sf::st_intersects(RL$geometry, SES$geometry)    
RL_CT_Intersect<-as.data.frame(RL_CT_Intersect)
colnames(RL_CT_Intersect)<- c("HOLCID","CENSUSTRACTID")
m<-merge(RL_CT_Intersect, RL, by = "HOLCID", all.y = TRUE)


m$holcnum<-ifelse(m$holc_grade=="A", 1, 
                  ifelse(m$holc_grade=="B", 2, 
                         ifelse(m$holc_grade=="C",3,
                                ifelse(m$holc_grade=="D",4,0))))

m %>% arrange(desc(holcnum))  %>% 
  group_by(CENSUSTRACTID) %>% 
  slice(1) %>%
  dplyr::select(CENSUSTRACTID, HOLCID, holc_grade, holcnum, city, state)-> m3

m %>% arrange(holcnum)  %>% #cut desc() for sensitivity analysis 
  group_by(CENSUSTRACTID) %>% 
  slice(1) %>%
  dplyr::select(CENSUSTRACTID, HOLCID, holc_grade, holcnum, city, state)-> m4

final_default<-merge(m3, SES, by = "CENSUSTRACTID", all.x = T)
final_alternate<-merge(m4, SES, by = "CENSUSTRACTID", all.x = T)

final_default %>% filter(!is.na(Data_Value)) -> final_default
final_alternate %>% filter(!is.na(Data_Value)) -> final_alternate

saveRDS(final_default, "FINAL_default.rds")
saveRDS(final_alternate, "FINAL_alternate.rds")
