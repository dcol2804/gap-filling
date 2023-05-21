
######################################################################

# This script requires a 3.4.4 version of R and the BHPMF package installed in that version
# Instructions can be found here: https://github.com/fisw10/BHPMF

# It logs the data, runs the model and stores the result. 

# You can alter the tuning parameter which increases the amount of time it takes to model
#
###########################################################################



library(BHPMF)

setwd("BHPMF")
tmp.dir <- dirname("output_data/SLA_comparison/tmp/")

data_original = read.csv("input_data/climate_vars.csv", stringsAsFactors = F)

data_original = data_original[, c("X", "taxon_name", "genus", "family", "leaf_mass_per_area", "seed_dry_mass", "leaf_length", "leaf_width", "plant_height")]

set.seed(123)

#Randomly assign NA to 10% of the data

data = data_original

data$original_leaf_mass_per_area = data$leaf_mass_per_area
data$leaf_mass_per_area[sample(which(!is.na(data$leaf_mass_per_area)), length(which(!is.na(data$leaf_mass_per_area))) * 0.10)] <-
NA



#data$subset_of_blanks = ifelse(is.na(data$leaf_mass_per_area)&!is.na(data_original$leaf_mass_per_area), "test", NA)

data = data[-which(is.na(data$leaf_mass_per_area)&
is.na(data$leaf_length)&
is.na(data$leaf_width)&
is.na(data$leaf_mass_per_area)&
is.na(data$seed_dry_mass)),]



write.csv(data, "output_data/SLA_comparison/original_data.csv", row.names= F)

data = data[, which(names(data) != "original_leaf_mass_per_area")]

hierarchy.info = data[,c(1, 2, 3, 4)]
trait.info.original = data[,c(5:length(data))]
trait.info = data[,c(5:length(data))]



#############
back_trans_pars <- list()

rm_col <- c()

for(i in 1:ncol(trait.info)){
  x <- trait.info[,i] # goes through the columns
  min_x <- min(x, na.rm = T) # takes the min of each column
  if(min_x < 0.00000000001){
    x <- x - min_x + 1 # make this optional if min x is neg
  }
  
  logx <- log10(x)
  mlogx <- mean(logx, na.rm = T)
  slogx <- sd(logx, na.rm = T)
  
  x <- (logx - mlogx)/slogx # Z transformation
  
  # store the params
  
  back_trans_pars[[i]] <- list(min_x = min_x,
                               mlogx = mlogx,
                               slogx = slogx)
  trait.info[,i] <- x
  
}

write.csv(trait.info, "output_data/SLA_comparison/transformed_original_2.csv", row.names = F)

trait.info = as.matrix(trait.info)
hierarchy.info = as.matrix(hierarchy.info)

GapFilling(trait.info, hierarchy.info, tuning = F,
           mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled_2.txt"),
           std.gap.filled.output.path= paste0(tmp.dir,"/std_gap_filled_2.txt"), tmp.dir=tmp.dir)


head(trait.info)

output = read.delim("output_data/SLA_comparison/mean_gap_filled_2.txt")

write.csv(output, row.names = F, "output_data/SLA_comparison/untransformed_model_output_2.csv")

# back transform the data
for (i in 1:ncol(output) ){
  
  x <- output[,i] # goes through the columns
  
  y = back_trans_pars[[i]]
  
  min_x = y[[1]]
  mlogx = y[[2]]
  slogx = y[[3]]
  
  x = 10^(x*slogx + mlogx)
  
  if(min_x < 0.00000000001){
    x <-  x + min_x - 1 # make this optional if min x is neg
  }
  
  output[,i] <- x
  
}

write.csv(output, row.names = F, "output_data/SLA_comparison/transformed_model_output_2.csv")

output2 = as.data.frame(read.delim("output_data/SLA_comparison/std_gap_filled_2.txt"))

write.csv(output2, row.names = F, "output_data/SLA_comparison/untransformed_model_std_2.csv")

# back transform the data
for (i in 1:ncol(output2) ){
  
  x <- output2[,i] # goes through the columns
  
  y = back_trans_pars[[i]]
  
  min_x = y[[1]]
  mlogx = y[[2]]
  slogx = y[[3]]
  
  x = 10^(x*slogx + mlogx)
  
  if(min_x < 0.00000000001){
    x <-  x + min_x - 1 # make this optional if min x is neg
  }
  
  output2[,i] <- x
  
}

write.csv(output2, row.names = F, "output_data/SLA_comparison/transformed_model_std_2.csv")


# If you want to calculate the total RMSE separately, uncomment the following line and run
# It can take a while.
#######################################
# out1 <- CalculateCvRmse(trait.info, hierarchy.info)
avg.rmse <- out1$avg.rmse
std.rmse <- out1$std.rmse
write.csv(as.data.frame(out1), "output_data/rmse.csv")
