# US Map
# Erin J Campbell
# Sept 2021

## This code builds the city-by-city regression results map
## It utilizes sheet 9 of the xlsx sheet included in this folder

library(tidyverse)
library(readxl)
library(sf)
library(rgeos)
library(maps)
library(ggrepel)
library(ggspatial)
library(rnaturalearth)
library(ggpubr)

#################################################################################################################################  

d2 <- read_xlsx("D:\\Packaged Code\\Figure 3\\mapdata.xlsx",
               sheet = "Asth CbC")

d2$D <- format(round(d2$D, 2), nsmall = 2)

d2 <- d2 %>% select(Longitude, Latitude, everything())

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

linebreak <- "\n"

d2$CityLabel <- paste(d2$City, linebreak , d2$D, d2$`D: 95% CI`, sep = " ")
d2$D <- as.numeric(d2$D)

#manually making top and bottom 5 observations BOLD
d2$font <- ifelse(d2$D >= 1.6 | d2$D < -0.04, "bold", "plain")

asth<-ggplot(data = world) + 
  geom_sf() +
  geom_sf(data = states) +
  geom_label_repel(data = d2, 
                  aes(x = Longitude, y = Latitude, label = CityLabel),
                  fontface = d2$font, size = 2, max.overlaps = 32, 
                  force = 13)+
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE)+
  annotation_scale()+
  ggtitle("Panel B: Associations between HOLC grades and adult asthma prevalence \n (D graded tracts relative to A and B graded tracts)") 

####################################################################################

d3 <- read_xlsx("D:\\Packaged Code\\Figure 3\\RHI_citybycity.xlsx",
                sheet = "Sheet1")



d3$meandiff <- format(round(d3$meandiff, 2), nsmall = 2)
d3$CI_Low <- format(round(d3$CI_Low, 2), nsmall = 2)
d3$CI_high <- format(round(d3$CI_high, 2), nsmall = 2)

d3$CI_high <-trimws(d3$CI_high)
d3$CI_Low <-trimws(d3$CI_Low)


d3$CI <- paste(d3$CI_Low, d3$CI_high, sep = ", ")
d3$CI <- paste("(", d3$CI, ")")
  
d3 <- d3 %>% dplyr::select(Longitude, Latitude, everything())

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

linebreak <- "\n"

d3$CityLabel <- paste(d3$City, linebreak , d3$meandiff, sep = " ")
d3$meandiff<- as.numeric(d3$meandiff)

#manually making top and bottom 5 BOLD
d3$font <- ifelse(d3$meandiff >= 17 | d3$meandiff <= -2.3, "bold", "plain")


rhi<-ggplot(data = world) + 
  geom_sf() +
  geom_sf(data = states) +
  geom_label_repel(data = d3, 
                   aes(x = Longitude, y = Latitude, label = CityLabel),
                   fontface = d3$font, 
                   size = 2, 
                   max.overlaps = 32)+
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE)+
  annotation_scale(line_width = 0.5)+
  ggtitle("Panel A: Mean percent difference in the respiratory hazard index by \n HOLC grade (D graded tracts relative to A and B graded tracts)")

#################################################################################

ggarrange(rhi, asth, nrow = 2)
