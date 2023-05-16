
#############################################################
# This script creates some basic summary plots and stats to show the success 
# (or failure) of the modelling process 
#############################################################


library(tidyverse)

# read in the data

original = read.csv("input_data/summarised_austraits_traits.csv")

#log transformed
original_t = read.csv("output_data/transformed_original.csv")

original_t = cbind(original %>% select(family, genus, taxon_name), original_t)

modelled = read.csv("output_data/transformed_model_output.csv")

modelled_raw = read.csv("output_data/untransformed_model_output.csv")

modelled_sd = read.csv("output_data/transformed_model_std.csv")

modelled_sd_raw = read.csv("output_data/untransformed_model_std.csv")





# Look at each trait one by one

traits = names(modelled)



data = list()

for (i in 1:length(traits)){
  
  x = data.frame( original_t[traits[i]], 
                 modelled_raw[traits[i]],
                  modelled_sd_raw[traits[i]])
  names(x) = c(str_c("original_", traits[i]), str_c("modelled_", traits[i]), str_c("modelled_sd_", traits[i]))
  
  # make a column looking at the diff between the modelled and unmodelled
  diff = abs(x[1] - x[2])
  names(diff) = "difference"
 
  x = cbind(original %>% select(family, genus, taxon_name), x, diff)
  
  x = list(x)
  
  names(x) = traits[i]
  
  data = append(data, x)
  
}

# take out observations which are outside the maximum and minimum original values and plot

y = data[["leaf_thickness"]]

max = max(y$original_leaf_thickness, na.rm = T)
min = min(y$original_leaf_thickness, na.rm = T)

y1 = y %>% filter(modelled_leaf_thickness > min) 
y1 = y1 %>% filter(modelled_leaf_thickness < max) #%>% filter(!is.na(original_leaf_mass_per_area))
plot(y1$original_leaf_thickness, y1$modelled_leaf_thickness)

# I want to get the highest differences between modelled and real data and then look at how many datapoints we have in a genus
y2 = y1 %>% filter(!is.na(original_leaf_thickness)) %>% group_by(family) %>% summarise(n = n(), mean = mean(original_leaf_thickness), sd = sd(original_leaf_thickness), mean_diff = mean(difference), mean_uncert = mean(modelled_sd_leaf_thickness))
y2$ratio = y2$sd/y2$n
plot(y2$n, y2$mean_diff)
plot(y2$n, y2$mean_uncert)
length(which(is.na(y$original_leaf_thickness)))

# Look at the R2

rsq <- function(x, y) summary(lm(y~x))$r.squared
#SLA
rsq(y1$original_leaf_thickness, y1$modelled_leaf_thickness)



