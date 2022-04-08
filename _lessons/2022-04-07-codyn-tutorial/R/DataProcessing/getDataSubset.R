library(dplyr)

treatment = tibble(catchment =             c("34M",  "34U",    "34L"),
                   `Logging Intensity`   = c("none", "medium", "high"))

yearAnnot = tibble(
  year = 1995:2001,
  trt  = c( rep("pre-logging", 2), "logging", rep("post-logging", 4))
)

getDataSubset <- function() {
  # Make a smaller, more palatable version of the dataset
  df = read.csv("Data/TLW_invertebrateDensity.csv") %>%
    tidyr::pivot_longer( "Aeshna":"Trichoptera", names_to = "Species", values_to = "Density" ) %>%
    mutate(
      Count = Density * 0.33, 
      Count = tidyr::replace_na(Count, 0)) %>%
    filter(year >= 1995, 
           year <= 2001, 
           # stringr::str_starts(catchment, stringr::fixed("34")), 
           month %in% c("june", "may")) %>%
    right_join(treatment) %>%
    right_join(yearAnnot)
  
  ## Find species that have 0 abudnance across all sites and years.
  NA_sp = df %>%
    group_by(Species) %>%
    summarize(Count = sum(Count, na.rm=T)) %>%
    filter(Count==0) %>%
    select(Species)
  
  ## Remove those species from the dataset
  df = df %>% 
    anti_join(NA_sp)
}

sumOverReplicates <- function(df = getDataSubset()) {
  ## Check replicate consistency
  if( df %>% group_by(catchment, year, Species) %>%
      summarise(n=sum(replicate)) %>%
      filter(n!=55) %>% nrow){
    warning("WARNING! Data contains missing replicates! -Egor")
  }
  
  ## If no warning, we can sum across replicates
  df = df %>% group_by(`Logging Intensity`, catchment, month, year, Species) %>%
    summarise(TotalCount=sum(Count))
}

# Rank species by abundance
rankSpecies <- function(df = sumOverReplicates()) {
  df %>%
    ungroup() %>%
    group_by(year, catchment, month, `Logging Intensity`) %>%
    arrange(desc(TotalCount), .by_group = T) %>%
    mutate( Rank = seq_along(Species) )
}
