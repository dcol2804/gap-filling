library(V.PhyloMaker2)
library(dplyr)
library(Rphylopars)
library(ggplot2)
library(austraits)
library(tidyverse)

austraits <- load_austraits(path="austraits_database/austraits", version = "4.1.0")

# Filter to just the traits you want
aus = left_join(austraits$traits, austraits$taxa %>% select("taxon_name", "genus", "family"), by = "taxon_name") %>% rename(trait_value = value)


df <-
  data.frame(
    species = aus$taxon_name,
    genus = aus$genus,
    family = aus$family
  )
df <- distinct(df)
# tree <- phylo.maker(df)
# saveRDS(tree,"autraitstree.rds")
readRDS("Rphyloparse/downloads/autraitstree.rds")
phylo <- tree[[1]]

lma <- filter(aus, trait_name == "plant_height") %>%
  select(taxon_name, trait_value) %>%
  mutate(trait_value2 = log10(as.numeric(trait_value))) %>%
  select(taxon_name, trait_value2) %>%
  group_by(taxon_name) %>%
  summarize(trait_value2 = mean(trait_value2))

lma_vec <- data.frame(species = lma$taxon_name,
                      lma.log = lma$trait_value2)
rownames(lma_vec) <- gsub(" ", "_", lma$taxon_name)
lma_vec$species <- gsub(" ", "_", lma_vec$species)
lma_vec <- filter(lma_vec, lma_vec$species %in% phylo$tip.label)

# Run phylogenetic comparative analysis
phylopars_result <-
  phylopars(trait_data = lma_vec,
            tree = phylo,
            model = "BM")

# Set the seed for reproducibility
set.seed(123)

# Randomly assign NA to 10% of the data
lma_vec_missing <- lma_vec
lma_vec_missing$lma.log[sample(nrow(lma_vec), nrow(lma_vec) * 0.10)] <-
  NA

# Run phylogenetic comparative analysis
phylopars_result_missing <-
  phylopars(trait_data = lma_vec_missing,
            tree = phylo,
            model = "BM")

# Extract the estimated values
estimated_values <- phylopars_result_missing$anc_recon

rownames_to_column(data.frame(estimated_values), var = "species") %>%
  filter(species %in% lma_vec_missing$species) %>%
  rename(estimated_lma_log = lma.log) -> est

left_join(lma_vec_missing, est) %>%
  rename(lma_with_missing = lma.log) -> out

left_join(out, lma_vec) -> out

out %>%
  filter(is.na(lma_with_missing)) -> missing_data

missing_data %>%
  ggplot(aes(x = estimated_lma_log, y = lma.log)) + geom_point()

summary(lm(lma.log ~ estimated_lma_log, data = missing_data))
