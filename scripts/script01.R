# Heading -----------------------------------------------------------------

## Data Visualization (GOVT16) Summer 2019
## Work-On-Your-Own Data Visualization, Project 2
## 
## Name: Sherry Liu
## Date: July 31, 2019


# Load --------------------------------------------------------------------

library(tidyverse)

# Read ---------------------------------------------------------------

old <- readRDS("data/V-Dem-CY-Core-v9.rds")

# Data Wrangle ------------------------------------------------------------

africa <- df %>% 
  filter(e_regiongeo %in% c(5:9))

View(names(df))

varname <- names(df)


func <- function(name){
  print(name %in% varname)
}


# does it exist -----------------------------------------------------------

func("v2x_genpp")
func("v2x_gencs")
func("e_miferrat")
func("e_regiongeo")
func("v2fsuffrage")
func("e_v2x_suffr")
func("v2elsuffrage")

# aaaa --------------------------------------------------------------------

aaaa<- readRDS("data/V-Dem-CY-Full+Others-v9.rds")
worldmap <- read.csv("data/HiResWorldMapWithISO3.csv")

nameaaaa <- names(aaaa)

View(aaaa2)

aaaa2 <- aaaa %>% 
  select(country_name, country_text_id, year, e_regiongeo, 
         v2asuffrage, v2elsuffrage, v2fsuffrage, v2msuffrage, 
         e_miferrat, v2x_genpp, v2clacjstm, v2clacjstw, 
         v2clprptym, v2clprptyw) %>% 
  filter(year == 2018) %>% 
  rename(name = country_name, id = country_text_id, deFactoVote = v2asuffrage,
         deJuroVote = v2elsuffrage, female = v2fsuffrage, male = v2msuffrage,
         region = e_regiongeo, fertility = e_miferrat, 
         femPolitParticipate = v2x_genpp, 
         accessJusticeM = v2clacjstm, accessJusticeF = v2clacjstw,
         propRightsM = v2clprptym, propRightsW = v2clprptyw)

View(aaaa2)

ggplot() + 
  geom_polygon(data = worldmap, aes(x = long, y = lat, group = group, 
                                    color = "aaaaaaa"))

  










