# Heading -----------------------------------------------------------------

## Data Visualization (GOVT16) Summer 2019
## Work-On-Your-Own Data Visualization, Project 2
## 
## Name: Sherry Liu
## Date: July 31, 2019


# Load --------------------------------------------------------------------

library(tidyverse)


# Read --------------------------------------------------------------------

fullDem<- readRDS("data/V-Dem-CY-Full+Others-v9.rds")
worldmap <- read.csv("data/HiResWorldMapWithISO3.csv")


#https://www.prio.org/Data/Geographical-and-Resource-Datasets/Length-of-International-Boundaries/


# VDem --------------------------------------------------------------------

vdem1 <-fullDem %>% 
  select(country_name, country_text_id, e_regiongeo, 
         e_wb_pop, e_migdppc,
         year, codingstart, codingend, 
         v2x_polyarchy, v2x_libdem, e_chga_demo) %>% 
  rename(name = country_name, id = country_text_id, region = e_regiongeo,
         electoral = v2x_polyarchy, liberal = v2x_libdem, 
         democracy = e_chga_demo, population = e_wb_pop,
         gdpPC = e_migdppc) %>% 
  filter(codingend == 2018, year == 2008, region %in% 1:4, !is.na(democracy)) %>% 
  mutate(independenceYears = codingend - codingstart) %>% 
  mutate(GDP = gdpPC * population) %>% 
  mutate(demIND = (electoral + liberal) / 2) 

# wrangle2 ----------------------------------------------------------------


# vdem1 <- vdem1 %>% 
#   ##region -> change levels
#   mutate(region = 
# 
#            View(vdem1)
# vdem1$region
# labels(vdem1["region"])
# levels(vdem1["region"])
# )
  
##group_by
##summarize
##
  
  


  
rm(fullDem)



# Worldmap -----------------------------------------------------------------

borderFind <- function(countryCode){
  
  undemBorder <- 0
  total <- 0
  
  for(eachCountry in table(worldmap$id)){
    
    ## countries are in order
    if( ##[point1] -> [point2] is shared by two countries)
    )
      dist <- sqrt(((lat1) - (lat2))^2 + ((long1) - (long2))^2)
  
      if(vdem1[eachCountry, democratic] == 0){
        undemBorder <- undemBorder + dist
      }
      total <- total + dist
  }
  demDF <- rbind(countryCode, undemBorder/total)
  return (demDF)
}



demDF <- data.frame(id = character(), borderPorp = numeric())

for(eachCountry in table(worldmap$id)){
  borderFind(eachCountry)
}


table(worldmap$id)

# Join --------------------------------------------------------------------

vdem2 <- left_join(vdem1, demDF, by = "id")
  
  
# Plot --------------------------------------------------------------------

ggplot() + 
  geom_point(data = vdem2, mapping = aes(x = ????, 
                                         y = demInd, 
                                         size = GDP,
                                         colour = region)) + 
  annotate("text", x = ???, y = ???,
           label = "Country1") +
  annotate("text", x = ???, y = ???,
           label = "Country2") + 
  annotate("text", x = ???, y = ???,
           label = "Germany") +
  geom_hline(slope = .5, 
             color = "grey", 
             linetype = "dashed") + 
  theme_minimal() +
  theme(legend.position = "right") +
  scale_x_reverse() + 
  labs(title = "Shared Borders on Democracy (2018)",
       subtitle = "also showing population and region"
       x = "Border shared with Undemocratic Countries (%)", 
       y = "Democracy Index", 
       colour = "Region"
       caption = "Data from VDem") 
  




# e -----------------------------------------------------------------------

  
ggplot() + 
  geom_point(data = vdem1, mapping = aes(x = id,
                                             y = demIND, 
                                             size = independenceYears,
                                             colour = as.factor(region),
                                         alpha = 8)) 
  
  

# shared coastline----------------------------------------------------------------


# mex ---------------------------------------------------------------------

USmap <- worldmap %>% 
  filter(id == "USA")

MEXmap <- worldmap %>% 
  filter(id == "MEX")

View(USmap)
View(MEXmap)

USMex <- (left_join(USmap, MEXmap, by = "lat"))
View(a123)
USMex <- filter(USMex, !is.na(group.y))




# rus ---------------------------------------------------------------------

border <- 0
total <- 0

RusMap <- worldmap %>% 
  filter(id == "RUS") %>% 
  select(lat, long) %>% 
  mutate(same = 1)

FinMap <- worldmap %>% 
  filter(id == "FIN") 

FinRus <- (left_join(FinMap, RusMap, by = c("lat", "long"))) %>% 
  filter(same == 1)

dist <- sqrt(((FinRus["lat"][1,]) - (FinRus["lat"][2,]))^2 + 
               ((FinRus["long"][1,]) - (FinRus["long"][2,]))^2)


vdem2 <- vdem1 %>% 
  filter(id == "RUS") %>% 
  select(democracy)

if (vdem2$democracy == 0) {
  border <- border + dist
}

total <- total + dist




table(worldmap$id)


# hide --------------------------------------------------------------------





wo2rldmap <- worldmap %>% 
  filter(id %in% c("UKR", "RUS")) 
  

ggplot() + 
  geom_path(data = wo2rldmap, aes(x = long, y = lat, group = group, fill = id)) + 
  labs(title = "perkell")


View (wo2rldmap)




# eeee --------------------------------------------------------------------

demDF <- data.frame(id = character(), borderPorp = numeric())

worldmap3 <- worldmap %>% 
  count(id)



View(worldmap3)

for(id in worldmap3)){
  borderFind(id)
}

borderFind <- function(mainCountry){
  
  undemBorder <- 0
  total <- 0
  
  for(eachCountry in (worldmap3)){
    if (eachCountry != mainCountry){
      
      BorderMap <- worldmap %>% 
        filter(id == eachCountry) %>% 
        select(lat, long) %>% 
        mutate(same = 1)
      
      MainMap <- worldmap %>% 
        filter(id == mainCountry)
      
      joint <-(left_join(MainMap, BorderMap, by = c("lat", "long"))) %>% 
        filter(same == 1)

      for (i in 1:(nrow(joint)-1)){
        dist <- sqrt(((joint["lat"][i,]) - (joint["lat"][i+1,]))^2 + 
                       ((joint["long"][i,]) - (joint["long"][i+1,]))^2)
      }
      
      isDem <- vdem1 %>% 
        filter(id == eachCountry) %>% 
        select(democracy)
      
      if (vdem2$democracy == 0) {
        border <- border + dist
      }
    
    total <- total + dist
    }
  }
      
  demDF <- rbind(countryCode, border/total)
  return (demDF)
}


# Border Test: Finland and Russia -------------------------------------------

# border <- 0
# total <- 0
# 
# RusMap <- worldmap %>% 
#   filter(id == "RUS") %>% 
#   select(lat, long) %>% 
#   mutate(same = 1)
# 
# FinMap <- worldmap %>% 
#   filter(id == "FIN") 
# 
# FinRus <- (left_join(FinMap, RusMap, by = c("lat", "long"))) %>% 
#   filter(same == 1)
# 
# dist <- sqrt(((FinRus["lat"][1,]) - (FinRus["lat"][2,]))^2 + 
#                ((FinRus["long"][1,]) - (FinRus["long"][2,]))^2)
# 
# 
# vdem2 <- vdem1 %>% 
#   filter(id == "RUS") %>% 
#   select(democracy)
# 
# if (vdem2$democracy == 0) {
#   border <- border + dist
# }
# 
# total <- total + dist


# Border Calculation Function -----------------------------------------------------

# borderFind <- function(mainCountry){
#   
#   border <- 0
#   total <- 0
#   
#   for(eachCountry in (vdem1$id)){
#     print(paste(eachCountry, "For loop"))
#     if (eachCountry != mainCountry){
#       print("If 1 started")
#       
#       BorderMap <- worldmap %>%
#         filter(id == eachCountry) %>%
#         select(lat, long, id) %>%
#         mutate(same = 1)
#       print("Border Map Done")
#       
#       MainMap <- worldmap %>%
#         filter(id == mainCountry)
#       
#       joint <- left_join(MainMap, BorderMap, by = c("long", "lat")) %>%
#         filter(same == 1)
#       print("Joint Done")
#       
#       if(nrow(joint)!= 0){
#         for (i in 1:(nrow(joint)-1)){
#           print(paste(nrow(joint), "rows in", eachCountry))
#           dist <- sqrt(((joint["lat"][i,]) - (joint["lat"][i+1,]))^2 +
#                          ((joint["long"][i,]) - (joint["long"][i+1,]))^2)
#           
#           
#           isDem <- vdem1 %>%
#             filter(id == eachCountry) %>%
#             select(democracy)
#           
#           if (isDem$democracy == 1) {
#             border <- border + dist
#           }
#           
#           total <- total + dist
#         }
#       }
#     }
#   }
#   demDF <- rbind(data.frame(id = mainCountry, borderPorp = border/total))
# }
# 
# 
# demDF <- data.frame(id = character(), borderPorp = numeric())
# 
# for(id in vdem1$id){
#   borderFind(id)
# }





