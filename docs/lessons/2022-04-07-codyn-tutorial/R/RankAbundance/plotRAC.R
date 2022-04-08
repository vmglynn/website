library(ggplot2)
source("R/DataProcessing/getDataSubset.R")

plotRAC <- function(df = sumOverReplicates, facets=T) {
  plot = df %>%
    ggplot(aes(x=Rank, y=TotalCount, color=`Logging Intensity`)) + 
    xlab("Rank") + 
    ylab("log Total count (sum of replicates)") + 
    geom_line(aes(size=`Logging Intensity`)) + 
    scale_size_manual(values=c(2.5, 2, 1.5)) +
    theme_classic(base_size=16) +
    scale_y_log10() +
    scale_color_brewer(palette="Dark2")
  
  if(facets){
    plot= plot +facet_wrap(vars(year))
  }
  plot
}

# plotRAC()
