
######################################################################

# This script requires a 3.4.4 version of R and the BHPMF package installed in that version
# Instructions can be found here: https://github.com/fisw10/BHPMF

# It logs the data, runs the model and stores the result. 

# You can alter the tuning parameter which increases the amount of time it takes to model
#
###########################################################################



library(BHPMF)

setwd("BHPMF")
tmp.dir <- dirname("output_data/tmp/")

data = read.csv("input_data/summarised_austraits_traits.csv", stringsAsFactors = F)

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

write.csv(trait.info, "output_data/transformed_original.csv", row.names = F)

trait.info = as.matrix(trait.info)
hierarchy.info = as.matrix(hierarchy.info)

GapFilling(trait.info, hierarchy.info, tuning = F,
           mean.gap.filled.output.path = paste0(tmp.dir,"/mean_gap_filled.txt"),
           std.gap.filled.output.path= paste0(tmp.dir,"/std_gap_filled.txt"), tmp.dir=tmp.dir)


head(trait.info)

output = read.delim("output_data/mean_gap_filled.txt")

write.csv(output, row.names = F, "output_data/untransformed_model_output.csv")

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

write.csv(output, row.names = F, "output_data/transformed_model_output.csv")

output2 = as.data.frame(read.delim("output_data/std_gap_filled.txt"))

write.csv(output2, row.names = F, "output_data/untransformed_model_std.csv")

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

write.csv(output2, row.names = F, "output_data/transformed_model_std.csv")

# I want to plot the known versus the predicted values for each trait 
# To get the known value, I'll get the column in "original. To get the predicted, I'll get the trait info.
# make the vector of predicted plant heights that we know them for. 
output = cbind(hierarchy.info, output)

# The only ones in their family often have crazy values

# If you want to calculate the total RMSE separately, uncomment the following line and run
# It can take a while.
#######################################
# out1 <- CalculateCvRmse(trait.info, hierarchy.info)
avg.rmse <- out1$avg.rmse
std.rmse <- out1$std.rmse
write.csv(as.data.frame(out1), "output_data/rmse.csv")
