
#########################################################################
######## This script loads Austraits data into R 
######## Loads a csv containing the traits you want for the BHPMF
######## Creates a summary value for each taxon-trait combination
######## Adds on necessary taxonomic hierarchy to each species

######## IMPORTANT: The summarising process can be quite lengthy depending on the number of datapoints
######## Probably best to let it run in the background or overnight

#########################################################################



# Load packages
#remotes::install_github("traitecoevo/austraits")
library(austraits)
library(tidyverse)

# Set your working directory to the data folder

# Load the csv file with the traits you would like to model
x = read.csv("BHPMF/input_data/trait_list.csv")

# Load austraits
austraits <- load_austraits(path="austraits_database/austraits", version = "4.1.0")

# Filter to just the traits you want
aus = austraits$traits %>% filter(trait_name %in% x$traits) %>% rename(trait_value = value)

# get number of known taxa
taxa = aus %>% select(taxon_name, trait_name) %>% unique() %>% group_by(trait_name) %>% summarise(n = n())

# check to see if the traits you want are written correctly in the spreadsheet
x$traits[!x$traits %in% unique(aus$trait_name)]

# Add on the taxonomic information to only select species level taxa
aus = aus %>% left_join(austraits$taxa %>% select(taxon_rank, taxon_name, genus, family))
aus = aus %>% filter(taxon_rank == "Species")

aus = aus %>% filter(!trait_name %in% unique(names(x)))

#################################################################################
##################### create a summary value for each ###########################
################################################################################

# Make a step here where you check to see if the mean values for taxa have already been calculated and stored!

# Create a reference list of taxon-trait combinations and arrange them by ranking
num_traits = aus %>% select(taxon_name, trait_name, unit) %>% unique()

if( nrow(num_traits) != 0){
  
  #create a blank dataframe
  output1 = data.frame()
  
  # for each of the numeric traits...
  for (i in 1:length(num_traits$trait_name)){
    
    # get the data for each numeric trait, group by the value type and find the mean
    # remove wrongly entered data and get numeric data only data only
    a = aus %>% filter(taxon_name == num_traits$taxon_name[i]) %>%
      filter(trait_name == num_traits$trait_name[i]) %>%
      mutate(trait_value = as.numeric(trait_value))
    
    
    ##############################################################################################
    
    # create a dataset of unknown sites
    # calculate for mean and raw. Doesn't matter whether they have a location_d or not - the NA will count as a location and the dataset_id is unique
    data_means = a %>% filter(value_type %in% c("mean", "raw"))  %>% group_by(dataset_id) %>% summarise(mean = mean(trait_value))
    
    # Now create a mean via the expert mins and maxes
    expert = a %>% filter(value_type %in% c("minimum", "maximum"), basis_of_record %in% c("preserved_specimen", "literature"))
    
    # make an average if a range is given
    experts = expert %>% group_by(dataset_id) %>% summarise(mean = mean(trait_value), n = n())
    # make sure your mean comes from at least two observations. Should never be three because why would you have more than one estimate of a max or min per dataset?
    experts = experts %>% filter(n == 2) %>% select(-n)
    # stick them together with the other sites
    site_means = rbind(data_means, experts)
    
    # take the mean, min and max of sites
    overall_mean = data.frame(taxon_name = num_traits$taxon_name[i],
                              
                              trait_name = num_traits$trait_name[i],
                              
                              mean = mean(site_means$mean, na.rm = T),
                              
                              unit = num_traits$unit[i]
                              
    )
    
    
    ##################### Min and Max temporary patch ##############################
    # If there's more than 1 observation:
    
    if (nrow(a) > 1 & length(unique(a$value_type)) > 1){
      
      overall_mean$min = min(a$trait_value)
      overall_mean$max = max(a$trait_value)
      
    }else if ( unique(a$value_type) == "maximum"){
      overall_mean$min = NA
      overall_mean$max = max(a$trait_value)
    }else{
      overall_mean$min = NA
      overall_mean$max = NA
    }
    
    # stick it to the previous observation
    output1 = rbind(output1, overall_mean)
    
    
  }
  
  ################################## Final changes before sending ##################
  
  output1 = output1 %>% select(taxon_name, trait_name, min, mean, max, unit)
  
  # I'm (incorrectly) rounding to 3 significant figures over all traits. A more sophisticated function is needed
  output1$mean = signif(as.numeric(output1$mean), 3)
  output1$min = signif(as.numeric(output1$min), 3)
  output1$max = signif(as.numeric(output1$max), 3)
  output1$mean[is.nan(output1$mean)] = NA
}

################################################################################
# ######## create a sparse matrix with taxonomic hierarchy ready for BHPMF #################
################################################################################

# make plant_height max = plant height mean so we don't lose most of the data 
# (in the max column) when we just take the means
output1$mean = ifelse(output1$trait_name == "plant_height", output1$max, output1$mean)
output1$mean = ifelse(output1$trait_name == "plant_diameter_breast_height", output1$max, output1$mean)

# Take out rows that are all blanks i.e. have no traits
output2 = output1 %>% filter(!(is.na(min) & is.na(mean) & is.na(max)))

# If you'd like to maximise the population of the taxon-trait matrix, 
# we could move any traits that provide only a max to the mean but for now remove them
output2 = output2 %>% filter(!is.na(mean))

# simplify
out = output2 %>% select(taxon_name, trait_name, mean)

out1 = out %>% pivot_wider(names_from = trait_name, values_from = mean)

out1 = out1 %>% left_join(austraits$taxa %>% select(taxon_name, genus, family))

out1 = out1 %>% select(taxon_name, genus, family, leaf_mass_per_area:leaf_lifespan)

# remove taxa without a family or with two families in Austraits (has to be done manually so as to select the correct family)
out1 = out1 %>% filter(!is.na(family), !is.na(genus))
test = out1 %>% select(genus, family) %>% unique()


test %>% filter(duplicated(genus))
out1 = out1 %>% filter(!taxon_name %in% c("Thismia sp. (Ty-Gwyn)",
                                         "Taraxacum sect. Erythrosperma",
                                         "Tetratheca sp. Freycinet Peninsula (A.C.Rozefelds 323)"))


out2 = x %>% full_join(out1, by = c("taxon_name", "genus", "family"))

# create an index column
out2$X = 1:nrow(out2)

# write the output into the data folder ready for modelling
write.csv(out2, "BHPMF/input_data/summarised_austraits_traits.csv", row.names = F)

