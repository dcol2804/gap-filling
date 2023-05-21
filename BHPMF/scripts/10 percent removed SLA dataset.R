library(tidyverse)
# Read in the logged results of _1 and _2 and the original data



original = read.csv("output_data/SLA_comparison/original_data.csv")
original$subset_of_blanks = ifelse(is.na(original$leaf_mass_per_area)&!is.na(original$original_leaf_mass_per_area), "test", NA)
original = original %>% select(-leaf_mass_per_area) %>% rename(leaf_mass_per_area = original_leaf_mass_per_area)

original_t = read.csv("output_data/SLA_comparison/transformed_original_2.csv")

modelled_raw = read.csv("output_data/SLA_comparison/untransformed_model_output_2.csv")

modelled = read.csv("output_data/SLA_comparison/transformed_model_output_2.csv")

modelled_sd_raw = read.csv("output_data/SLA_comparison/untransformed_model_std_2.csv")

traits = names(modelled)

data = list()

for (i in 1:length(traits)){
  
  x = data.frame( original[traits[i]],
                  original_t[traits[i]],
                  modelled[traits[i]],
                  modelled_sd_raw[traits[i]])
  names(x) = c("original", "original_transformed", "modelled", "modelled_sd")
  
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

z = data[["leaf_mass_per_area"]]

max = max(z$original, na.rm = T)
min = min(z$original, na.rm = T)

z = z %>% filter(modelled > min) 
z = z %>% filter(modelled < max) 


# Look at the R2

rsq <- function(x, y) summary(lm(y~x))$r.squared

###########################################################
summary(lm(log_original ~ log_modelled, data = subset_of_blanks))


subset_of_blanks = z[is.na(z$original_transformed)&!is.na(z$original),]

plot(subset_of_blanks$log_original, subset_of_blanks$log_modelled)

rsq(subset_of_blanks$log_original, subset_of_blanks$log_modelled)



