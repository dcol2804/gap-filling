# Load packages

library(V.PhyloMaker2)
library(Rphylopars)
library(tidyverse)


### Load Taxon-trait matrix

aus = read_csv("Assembling_trait_matrix/data/climate_vars.csv")


# Make the logged dataframe
aus = aus %>% mutate(plant_height.log = log10(plant_height),
                     seed_dry_mass.log = log10(seed_dry_mass),
                     leaf_mass_per_area.log = log10(leaf_mass_per_area),
                     leaf_length.log = log10(leaf_length),
                     leaf_width.log= log10(leaf_width),
                     species = gsub(" ", "_", taxon_name))

data_in = aus %>% select(taxon_name,
                         species,
                         plant_height.log, 
                         seed_dry_mass.log, 
                         leaf_mass_per_area.log, 
                         leaf_length.log, 
                         leaf_width.log)

# Code to create the phylogeny - only needs to be done once
#aus_taxa = left_join(austraits$traits, austraits$taxa %>% select("taxon_name", "genus", "family"), by = "taxon_name")
# taxa <-
#   data.frame(
#     species = aus_taxa$taxon_name,
#     genus = aus_taxa$genus,
#     family = aus_taxa$family
#   )
# 
# taxa <- distinct(taxa)


# tree <- phylo.maker(taxa)
# saveRDS(tree,"Rphylopars/downloads/autraitstree.rds")
tree = readRDS("Rphylopars/downloads/autraitstree.rds")
phylo <- tree[[1]]

data_in <- data_in %>% filter(species %in% phylo$tip.label) %>% select(-taxon_name)

# Run phylogenetic comparative analysis
phylopars_result <-
  phylopars(trait_data = data_in,
            tree = phylo,
            model = "BM")






#################################################
#############  take out 10%  ####################
#################################################

set.seed(123)

# Randomly assign NA to 10% of the data
lma_vec_missing <- data_in

sample(which(!is.na(data_in$leaf_mass_per_area.log)), length(which(!is.na(data_in$leaf_mass_per_area.log)))*0.1)

lma_vec_missing$leaf_mass_per_area.log[sample(which(!is.na(data_in$leaf_mass_per_area.log)), length(which(!is.na(data_in$leaf_mass_per_area.log)))*0.1)] <- NA

# Run phylogenetic comparative analysis
phylopars_result_missing <-
  phylopars(trait_data = lma_vec_missing,
            tree = phylo,
            model = "BM")

# Extract the estimated values
estimated_values <- phylopars_result_missing$anc_recon

rownames_to_column(data.frame(estimated_values), var = "species") %>%
  filter(species %in% lma_vec_missing$species) %>%
  rename(estimated_lma_log = leaf_mass_per_area.log) -> est

names(est) = c("species", 
               "m_plant_height.log", 
               "m_seed_dry_mass.log", 
               "m_estimated_lma_log", 
               "m_leaf_length.log", 
               "m_leaf_width.log")

left_join(lma_vec_missing, est) %>% rename(lma_with_missing = leaf_mass_per_area.log) -> out

left_join(out, data_in) -> out

out %>%
  filter(is.na(lma_with_missing)) -> missing_data

missing_data %>%
  ggplot(aes(x = m_estimated_lma_log, y = leaf_mass_per_area.log)) + geom_point()

summary(lm(leaf_mass_per_area.log ~ m_estimated_lma_log, data = missing_data))


