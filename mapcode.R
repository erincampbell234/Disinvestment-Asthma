## Erin Campbell
## Figures for Asthma x Redlining
## July 2021


library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)

setwd("D:\\Packaged Code\\Figure 2")
d<-readRDS("D:\\Packaged Code\\Figure 2\\FINAL_alternate.rds")

d$geometry<-st_as_sf(d$geometry)


nyc <- d %>% dplyr::filter(city == "Manhattan") 
san <- d %>% dplyr::filter( city == "San Antonio") 
por <- d %>% dplyr::filter(city == "Portland") 

# PANEL 1 -- HOLC GRADES

n1 <- ggplot(nyc$geometry, aes(fill = nyc$holc_grade))+
  geom_sf()+
  coord_sf()+
  annotation_scale(location = "tl", width_hint = 0.5) +
  scale_fill_manual("HOLC \nGrades", values = c("A" = "#8dd3c7",
                                                "B" = "#bebeda",
                                                "C" = "#ffffb3",
                                                "D" = "#fb8072")) +
  ggtitle("NYC")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

s1 <- ggplot(san$geometry, aes(fill = san$holc_grade))+
  geom_sf()+
  coord_sf()+
  annotation_scale(location = "br", width_hint = 0.5) +
  scale_fill_manual("HOLC \nGrades", values = c("A" = "#8dd3c7",
                                                "B" = "#bebeda",
                                                "C" = "#ffffb3",
                                                "D" = "#fb8072")) +
  ggtitle("San Antonio")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

p1 <- ggplot(por$geometry, aes(fill = por$holc_grade))+
  geom_sf()+
  coord_sf()+
  annotation_scale(location = "tr", width_hint = 0.5) +
  scale_fill_manual("HOLC \nGrades", values = c("A" = "#8dd3c7",
                                                "B" = "#bebeda",
                                                "C" = "#ffffb3",
                                                "D" = "#fb8072")) +
  ggtitle("Portland")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

panel1 <- ggarrange(n1, s1, p1, ncol = 3, common.legend = TRUE, legend = "left",
                    align = "hv")

# PANEL 2 -- ASTHMA

n2 <- ggplot(nyc$geometry, aes(fill = nyc$Data_Value))+
  geom_sf()+
  coord_sf() +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())+
  scale_fill_gradient("Asthma \nPrev.", low = "yellow", high = "red", na.value = NA)



s2 <- ggplot(san$geometry, aes(fill = san$Data_Value))+
  geom_sf()+
  coord_sf() +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank()) +
  scale_fill_gradient("Asthma \nPrev.", low = "yellow", high = "red", na.value = NA)


p2 <- ggplot(por$geometry, aes(fill = por$Data_Value))+
  geom_sf()+
  coord_sf()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())+
  scale_fill_gradient("Asthma \nPrev.", low = "yellow", high = "red", na.value = NA)

panel2 <- ggarrange(n2, s2, p2, ncol = 3, common.legend = TRUE, legend = "left",
                    align = "hv")


# PANEL 3 -- RHI


n3 <- ggplot(nyc$geometry, aes(fill = nyc$`Total Respiratory (hazard quotient)`))+
  geom_sf()+
  coord_sf()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())+
  guides(colour = guide_colourbar(reverse = TRUE))+
  scale_fill_gradient("RHI")

s3 <- ggplot(san$geometry, aes(fill = san$`Total Respiratory (hazard quotient)`))+
  geom_sf()+
  coord_sf()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())+
  guides(colour = guide_colourbar(reverse = TRUE))+
  scale_fill_gradient("RHI")

p3 <- ggplot(por$geometry, aes(fill = por$`Total Respiratory (hazard quotient)`))+
  geom_sf()+
  coord_sf()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())+
  guides(colour = guide_colourbar(reverse = TRUE))+
  scale_fill_gradient("RHI")

panel3 <- ggarrange(n3, s3, p3, ncol = 3, common.legend = TRUE, legend = "left", 
                    align = "hv")

final <- ggarrange(panel1, panel2, panel3, nrow = 3, align = "hv",
                   label.y = )
final
