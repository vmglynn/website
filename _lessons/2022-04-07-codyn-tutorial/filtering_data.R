### FILTERING AND CLEANING DATA FOR THE CODYN TUTORIAL

data  <- read_csv("Data/TLW_invertebrateDensity.csv") %>%
  pivot_longer( "Aeshna":"Trichoptera", names_to = "species", values_to = "density") %>%
  mutate(abundance = density * 0.33, 
         treatment = case_when(catchment %in% c("33") ~ "low", 
                               catchment %in% c("34U") ~ "medium", 
                               catchment %in% c("34L") ~ "high", 
                               catchment %in% c("34M") ~ "control", 
                               TRUE ~ "natural"), 
         date_yr_m = as.yearmon(paste0(year,month), "%Y %b")) %>%
  drop_na(abundance) %>% 
  filter(year >= 1995, 
         year <= 2001, 
         # stringr::str_starts(catchment, stringr::fixed("34")), 
         month %in% c("june", "may"))

# summing over treatments sampling events
data_sum<- data %>%
  filter(treatment  %in% c("low", "medium", "high", "control")) %>% 
  group_by(year, species, treatment, replicate) %>%
  summarise(abundance = sum(abundance))

# write into csv
write.csv(x=data_sum, file="stream_invertebrates.csv", row.names=FALSE)








