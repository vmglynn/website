pacman::p_load(codyn, tidyverse, patchwork, zoo)
data  <- read_csv("Data/TLW_invertebrateDensity.csv") %>%
  pivot_longer( "Aeshna":"Trichoptera", names_to = "species", values_to = "density") %>%
  mutate(abundance = density * 0.33, 
         treatment = case_when(catchment %in% c("33") ~ "low", 
                               catchment %in% c("34U") ~ "medium", 
                               catchment %in% c("34L") ~ "high", 
                               catchment %in% c("34M") ~ "control", 
                               TRUE ~ "natural"), 
         date_yr_m = as.yearmon(paste0(year,month), "%Y %b")) %>%
  drop_na(abundance)
#anonymising the species column 
data$group_id <- data %>% group_indices(species) 
#looks like 1995 and 2001 are stable across all the treatments, so i filtered by these
data %>%
  group_by(treatment) %>%
  summarise(min(year), max(year))
#trying to filter down to the top species to see how the diversity changed
#you can definitely see that some species dropped out in certain treatments which is cool 
subset <- data %>%
  filter(year %in% c(1995, 2001), 
         abundance > 100) 
subset %>%
  ggplot(aes(x=reorder(as.factor(group_id),-abundance), y=abundance, fill=as.factor(year)))+
  geom_col()+
  facet_wrap(~treatment)
#####################################################
#codyn package exploration
#needing to group by year 
#can't seem to get this to work 
subset_codyn <- subset %>%
  filter(treatment == "high") %>%
  group_by(year, species, abundance, replicate) %>%
  summarise(tot_abundance = sum(abundance))

x <- abundance_change(df=subset_codyn,
                      time.var = "year",
                      species.var = "species",
                      abundance.var = "abundance", 
                      replicate.var = "replicate")


xx<- RAC_change(df=catchment33,
                time.var = "year",
                species.var = "species",
                abundance.var = "abundance", 
                replicate.var = "replicate")

xxx <- RAC_difference(df=catchment33,
                      time.var = "year",
                      species.var = "species",
                      abundance.var = "abundance", 
                      replicate.var = "replicate")

ggplot(x, aes(x=species, y=change, color=change>0))+
  geom_boxplot()+
  coord_flip()

ggplot(xxx, aes(x=as.factor(year), y=richness_diff))+
  geom_boxplot()

########################
# Jory's Codyn exploration

## I want to explore all of the functions in codyn using this dataset to see which ones are interesting

#### calculates turnover between time periods #######
# I believe you can only have two time periods in one stream for this so I will need to filter
# maybe we should focus on two years for this dataset because a lot of the functions seem to need 2 time points?
# I will look at turnover between two time periods in the high treatment
# filtering by the high treatment, summing over years and catchments
subset_codyn_high <- data %>%
  filter(treatment == "high") %>%
  group_by(year, species, replicate) %>%
  summarise(tot_abundance = sum(abundance))

# this seems to work with multiple years
sp_turnover<-turnover(df=subset_codyn_high, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")

ggplot(sp_turnover)+
  geom_point(aes(x=total, y=year))
# I have no idea what this means
# different points are different replicates

######### calculates mean relative change in species rank abundances ##########
# I think this also requires two time periods for the same stream (or treatment)
# I am going to use the same subset for this
sp_rank_shift<-rank_shift(df=subset_codyn1, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")

# outputs year pairs with values

ggplot(sp_rank_shift)+
  geom_point(aes(y=year_pair, x=MRS))
# i also want to  add the average rank shift (mean of replicates) for each year pair on top

sp_rank_shift_mean<-sp_rank_shift %>% group_by(year_pair) %>% summarise(mean_MRS=mean(MRS))

MRS_plot_high<-ggplot()+
  geom_point(sp_rank_shift, mapping=aes(y=year_pair, x=MRS))+
  geom_point(sp_rank_shift_mean, mapping=aes(y=year_pair, x=mean_MRS), color="red", size=5)+
  labs(title="High intensity logging")

###### I also want to try this with medium intensity logging
subset_codyn_med <- data %>%
  filter(treatment == "medium") %>%
  group_by(year, species, replicate) %>%
  summarise(tot_abundance = sum(abundance))

sp_rank_shift2<-rank_shift(df=subset_codyn_med, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")

sp_rank_shift2_mean<-sp_rank_shift2 %>% group_by(year_pair) %>% summarise(mean_MRS=mean(MRS))

MRS_plot_med<-ggplot()+
  geom_point(sp_rank_shift2, mapping=aes(y=year_pair, x=MRS))+
  geom_point(sp_rank_shift2_mean, mapping=aes(y=year_pair, x=mean_MRS), color="red", size=5)+
  labs(title="Medium intensity logging")

###### And low intensity logging
subset_codyn_low <- data %>%
  filter(treatment == "low") %>%
  group_by(year, species, replicate) %>%
  summarise(tot_abundance = sum(abundance))

sp_rank_shift3<-rank_shift(df=subset_codyn_low, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")

sp_rank_shift3_mean<-sp_rank_shift3 %>% group_by(year_pair) %>% summarise(mean_MRS=mean(MRS))

MRS_plot_low<-ggplot()+
  geom_point(sp_rank_shift3, mapping=aes(y=year_pair, x=MRS))+
  geom_point(sp_rank_shift3_mean, mapping=aes(y=year_pair, x=mean_MRS), color="red", size=5)+
  labs(title="Low intensity logging")
# not really seeing a difference with the years logged and years not logged

####### Looking at a control stream
### I am going to look at only one control to keep it consistent (because the others only have one stream I think-although there might be two streams for high)
subset_codyn_ctrl <- data %>%
  filter(catchment=="42") %>%
  group_by(year, species, replicate) %>%
  summarise(tot_abundance = sum(abundance))

sp_rank_shift4<-rank_shift(df=subset_codyn_ctrl, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")

sp_rank_shift4_mean<-sp_rank_shift4 %>% group_by(year_pair) %>% summarise(mean_MRS=mean(MRS))

MRS_plot_ctrl<-ggplot()+
  geom_point(sp_rank_shift4, mapping=aes(y=year_pair, x=MRS))+
  geom_point(sp_rank_shift4_mean, mapping=aes(y=year_pair, x=mean_MRS), color="red", size=5)+
  labs(title="No logging")
# ok does look like less change in the community structure

# Put them all together
library(ggpubr)
ggarrange(MRS_plot_low, MRS_plot_med, MRS_plot_high, MRS_plot_ctrl)


############### Calculates rate change in a community over time ###########
# This requires only one stream but can be done for multiple years
# We can look at 34L, the high intensity stream first
# Will try with the year month data
class(data$date_yr_m)
# sweet, this is class yearmon, we will see if it works
# it does not work because the time variable has to be numeric so we have to sum by year again

#### High intensity logging
rate_change_high<-rate_change(df=subset_codyn_high, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")
rate_change_high$treatment<-"high"
# this is the rate of change over time for each replicate, so if we can get this value for each stream, average over replicates, and plot we can compare them

### Medium intensity logging
rate_change_med<-rate_change(df=subset_codyn_med, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")
rate_change_med$treatment<-"med"
### Low intensity logging
rate_change_low<-rate_change(df=subset_codyn_low, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")
rate_change_low$treatment<-"low"
##### Control
rate_change_ctrl<-rate_change(df=subset_codyn_ctrl, time.var="year", species.var="species", abundance.var="tot_abundance", replicate.var="replicate")
rate_change_ctrl$treatment<-"control"
### Put them all together
rate_change_streams<-rbind(rate_change_high, rate_change_med, rate_change_low, rate_change_ctrl)

### and plot
ggplot()+
  geom_point(rate_change_streams, mapping=aes(x=rate_change, y=treatment))+
 labs(title="Rate of change by treatment")
# because the rate of change can go into negtives it does not really make sense to find the average over streams

########## Calculating richness and evenness using a specified metric for a replicate ##############
?community_structure
# can calculate richness and simpson's evenness
# looks at community structure of one location over time, so I can calculate for each treatment and then compare across years for treatments

#### Calculate richness

richness_high<-community_structure(df=subset_codyn_high, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                   metric="SimpsonEvenness")
richness_high$treatment<-"high"
# this is the rate of change over time for each replicate, so if we can get this value for each stream, average over replicates, and plot we can compare them

### Medium intensity logging
richness_med<-community_structure(df=subset_codyn_med, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                   metric="SimpsonEvenness")
richness_med$treatment<-"medium"
### Low intensity logging
richness_low<-community_structure(df=subset_codyn_low, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                  metric="SimpsonEvenness")
richness_low$treatment<-"low"

##### Control
richness_ctrl<-community_structure(df=subset_codyn_ctrl, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                  metric="SimpsonEvenness")
richness_ctrl$treatment<-"control"

### Put them all together
richness_streams<-rbind(richness_high, richness_med, richness_low, richness_ctrl)

ggplot()+
  geom_point(richness_streams, mapping=aes(x=year, y=richness, color=treatment))
# might want to average the replicates

richness_streams_mean<-richness_streams %>% group_by(treatment, year) %>% summarise(mean_richness=mean(richness), mean_evenness=mean(SimpsonEvenness))

ggplot()+
  geom_point(richness_streams, mapping=aes(x=year, y=richness, color=treatment))+
#  geom_point(richness_streams_mean, mapping=aes(x=year, y=mean_richness, color=treatment), shape=3, size=5)+
  geom_line(richness_streams_mean, mapping=aes(x=year, y=mean_richness, color=treatment))

ggplot()+
  geom_point(richness_streams, mapping=aes(x=year, y=SimpsonEvenness, color=treatment))+
  geom_line(richness_streams_mean, mapping=aes(x=year, y=mean_evenness, color=treatment))

########### Calculating community diversity using different metrics ######
# here I can calcualte shannons and simpson's diversity index
?community_diversity


####### Shannon's diversity
### high intensity logging
shan_diversity_high<-community_diversity(df=subset_codyn_high, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                   metric="Shannon")
shan_diversity_high$treatment<-"high"

### med intensity logging
shan_diversity_med<-community_diversity(df=subset_codyn_med, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                         metric="Shannon")
shan_diversity_med$treatment<-"medium"

### low intensity logging
shan_diversity_low<-community_diversity(df=subset_codyn_low, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                         metric="Shannon")
shan_diversity_low$treatment<-"low"

### control
shan_diversity_ctrl<-community_diversity(df=subset_codyn_ctrl, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                         metric="Shannon")
shan_diversity_ctrl$treatment<-"control"

## put together
shan_diversity<-rbind(shan_diversity_ctrl, shan_diversity_high, shan_diversity_low, shan_diversity_med)

### find average
shan_diversity_mean<-shan_diversity %>% group_by(treatment, year) %>% summarise(mean_shan=mean(Shannon))

ggplot()+
  geom_point(shan_diversity, mapping=aes(x=year, y=Shannon, color=treatment))+
  geom_line(shan_diversity_mean, mapping=aes(x=year, y=mean_shan, color=treatment))+
  labs(title="Shannon's diversity by treatment and year")

####### Simpson's Inverse diversity
### high intensity logging
simp_diversity_high<-community_diversity(df=subset_codyn_high, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                         metric="InverseSimpson")
simp_diversity_high$treatment<-"high"

### med intensity logging
simp_diversity_med<-community_diversity(df=subset_codyn_med, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                        metric="InverseSimpson")
simp_diversity_med$treatment<-"medium"

### low intensity logging
simp_diversity_low<-community_diversity(df=subset_codyn_low, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                        metric="InverseSimpson")
simp_diversity_low$treatment<-"low"

### control
simp_diversity_ctrl<-community_diversity(df=subset_codyn_ctrl, time.var="year", abundance.var="tot_abundance", replicate.var="replicate",
                                         metric="InverseSimpson")
simp_diversity_ctrl$treatment<-"control"

## put together
simp_diversity<-rbind(simp_diversity_ctrl, simp_diversity_high, simp_diversity_low, simp_diversity_med)

### find average
simp_diversity_mean<-simp_diversity %>% group_by(treatment, year) %>% summarise(mean_simp=mean(InverseSimpson))

ggplot()+
  geom_point(simp_diversity, mapping=aes(x=year, y=InverseSimpson, color=treatment))+
  geom_line(simp_diversity_mean, mapping=aes(x=year, y=mean_simp, color=treatment))+
  labs(title="Simpson's Inverse diversity by treatment and year")

############### Community stability ##########################
?community_stability
# This calculates the stability of the overall community over time as the temporal mean/temporal standard deviation of aggregate species abundances

## High intensity logging
stability_high<-community_stability(df=subset_codyn_high, time.var="year", abundance.var="tot_abundance", replicate.var="replicate")
stability_high$treatment<-"high"

## medium intensity logging
stability_med<-community_stability(df=subset_codyn_med, time.var="year", abundance.var="tot_abundance", replicate.var="replicate")
stability_med$treatment<-"medium"

## Low intensity logging
stability_low<-community_stability(df=subset_codyn_low, time.var="year", abundance.var="tot_abundance", replicate.var="replicate")
stability_low$treatment<-"low"

## Control
stability_ctrl<-community_stability(df=subset_codyn_ctrl, time.var="year", abundance.var="tot_abundance", replicate.var="replicate")
stability_ctrl$treatment<-"control"

## put together
stability<-rbind(stability_ctrl, stability_high, stability_low, stability_med)

## find mean for each
stability_mean<-stability %>% group_by(treatment) %>% summarise(mean_stability=mean(stability))

## Plot
ggplot()+
  geom_point(stability, mapping=aes(y=treatment, x=stability, color=treatment))+
  geom_point(stability_mean, mapping=aes(y=treatment, x=mean_stability), shape=4, size=5)+
  labs(title="Stability by treatment")








