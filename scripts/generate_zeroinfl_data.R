##! Set the working directory to the top level folder of the repo

library(tidyverse)
library(spatstat)
library(sf)

set.seed(1)


#### Simluate a dataset with constant zero inflation, but with a count that varies with a predictor ####

lambdas = seq(0.1,5,by=0.03) # a range of lambdas (parameter for the Poisson count distribution)
vals = rpois(length(lambdas),lambdas) # generate the count values

## randomly make 33% of the values zero
random_indeces = sample(1:length(vals), size=floor(length(vals)/3),replace=FALSE)
vals[random_indeces] = 0

# store as a data frame
constant_zeroinfl = data.frame(predictor = lambdas,
                               tree_count = vals)
write.csv(constant_zeroinfl,"data/simulated_constant_zeroinfl.csv",row.names=FALSE)

#### Simluate a dataset where both the zero probability and the count vary with a predictor ####

vals = rpois(length(lambdas),lambdas) # a range of lambdas (parameter for the Poisson count distribution)
probs = seq(from=.33,to=1,length.out=length(lambdas)) # probability of zeros (set so that on average, 33% of the values get set to zero)
binomial = rbinom(length(lambdas),1,probs) # make random binomial draws to determine which count values get assigned zero
vals = vals*binomial

# Store as a data frame
changing_zeroinfl = data.frame(predictor = lambdas,
                               tree_count = vals)
write.csv(constant_zeroinfl,"data/simulated_variable_zeroinfl.csv",row.names=FALSE)



#### Simluate a dataset where both the zero probability and the count vary with a predictor, but in opposite directions ####
# Samea s the above code block, but reverse the probability sequence passed to rbinom

vals = rpois(length(lambdas),lambdas)
probs = seq(from=1,to=.33,length.out=length(lambdas))
binomial = rbinom(length(lambdas),1,probs) # about 33% of the values are zero
vals = vals*binomial
changing_zeroinfl_reverse = data.frame(predictor = lambdas,
                                       tree_count = vals)
write.csv(constant_zeroinfl,"data/simulated_reverse_variable_zeroinfl.csv",row.names=FALSE)




#### Generate a maybe zero-inflated dataset based on small plots that sample a clustered spatial distribution of trees ####

scale = 0.3 # each cluster has a radius of 0.3 m
kappas =  seq(from=0.001, to=0.04, by=0.00005) # the kappa parameter determines the density of clusters: prepare to simulate landscapes with different densities of cluters (and thus trees)  
plot_size = 7.7 # 60 sq m, equivalent of a regen plot (as coded here, it's a square plot)

sim_data = NULL

# For each kappa value (density of tree clusters), simulate a distribution of trees within the specified plot size
for(kappa in kappas) {
    
  # Generate a random spatial pattern
  points = rMatClust(kappa,scale,mu=8,win=owin(c(0,plot_size),c(0,plot_size)))
  # Count the number of points in it
  npoints_sample = points$n

  # compile into a growing list of point counts
  sim_data_point = data.frame(predictor = kappa * 1000, tree_count = npoints_sample) # store kappa as "predictor" (multiply by 1000 for ease of reading)
  sim_data = bind_rows(sim_data,sim_data_point)
    
}


ggplot(data=sim_data,aes(x=predictor,y=tree_count)) +
  geom_point(alpha=0.3,size=3,shape=16) +
  theme_bw(16)


median(sim_data$tree_count)

hist(sim_data$tree_count)

# tree counts at low values of predictor
sim_data_low_predictor = sim_data %>%
  filter(predictor < 15)

# tree counts at high values of predictor
sim_data_high_predictor = sim_data %>%
  filter(predictor > 25)

hist(sim_data_low_predictor$tree_count) # at low values of the predictor, there is a massive excess of zeros
median(sim_data_low_predictor$tree_count) # the median is zero

hist(sim_data_high_predictor$tree_count) # at high values of the predictor, the distribution is more even

write.csv(sim_data,"data/simulated_spatial_variable_zeroinfl.csv",row.names=FALSE)

