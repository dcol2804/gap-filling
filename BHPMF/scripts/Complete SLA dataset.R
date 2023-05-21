library(tidyverse)
# Read in the logged results of _1 and _2 and the original data

#original = read.csv("input_data/SLA_summarised_austraits_traits.csv")
original = data
original = original[-which(is.na(original$leaf_mass_per_area)&
                     is.na(original$leaf_length)&
                     is.na(original$leaf_width)&
                     is.na(original$plant_height)&
                     is.na(original$leaf_area)&
                     is.na(original$wood_density)&
                     is.na(original$leaf_mass_per_area)&
                     is.na(original$leaf_N_per_dry_mass)&
                     is.na(original$seed_dry_mass)&
                     is.na(original$leaf_P_per_dry_mass)&
                     is.na(original$leaf_C_per_dry_mass)&
                     is.na(original$plant_diameter_breast_height)&
                     is.na(original$leaf_lifespan)),]

original_t = read.csv("output_data/complete/transformed_original_2.csv")

modelled_raw = read.csv("output_data/complete/untransformed_model_output_2.csv")

modelled = read.csv("output_data/complete/transformed_model_output_2.csv")

modelled_sd_raw = read.csv("output_data/complete/untransformed_model_std_2.csv")

traits = names(modelled)

data = list()

for (i in 1:length(traits)){
  
  x = data.frame( original[traits[i]], 
                  modelled[traits[i]],
                  modelled_sd_raw[traits[i]])
  names(x) = c("original", "modelled", "modelled_sd")
  
  # make a column looking at the diff between the modelled and unmodelled
  diff = abs(x[1] - x[2])
  names(diff) = "difference"
  
  x = cbind(original %>% select(family, genus, taxon_name), x, diff)
  
  x$log_original = log10(x$original)
  x$log_modelled = log10(x$modelled)
  x = list(x)
  
  names(x) = traits[i]
  
  data = append(data, x)
  
}

y = data[["leaf_mass_per_area"]]

max = max(y$original, na.rm = T)
min = min(y$original, na.rm = T)

y1 = y %>% filter(modelled > min) 
y1 = y1 %>% filter(modelled < max) #%>% filter(!is.na(original_leaf_mass_per_area))
plot(y1$log_original, y1$log_modelled)

# I want to get the highest differences between modelled and real data and then look at how many datapoints we have in a genus
y2 = y1 %>% filter(!is.na(original_leaf_mass_per_area)) %>% group_by(family) %>% summarise(n = n(), mean = mean(original_leaf_mass_per_area), sd = sd(original_leaf_mass_per_area), mean_diff = mean(difference), mean_uncert = mean(modelled_sd_leaf_mass_per_area))
y2$ratio = y2$sd/y2$n
plot(y2$n, y2$mean_diff)
plot(y2$n, y2$mean_uncert)
length(which(is.na(y$original_leaf_mass_per_area)))

# Look at the R2

rsq <- function(x, y) summary(lm(y~x))$r.squared

#SLA
rsq(y1$original_leaf_mass_per_area, y1$modelled_leaf_mass_per_area)


