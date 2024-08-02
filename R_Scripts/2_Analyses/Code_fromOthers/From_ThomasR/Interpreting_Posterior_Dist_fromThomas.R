str(det_mod_global)
test <- extract(det_mod_global)
test$beta_det

# Look at the proportion that is on the same size of zero as the mean (derived from the summary of the model)
# make a violin plot (package vioplot I think)
# If the percent of posterior samples that fall on the same of zero as the mean is >90%, you have reason to believe there's a strong effect (F value in jags)
# don't go down to 70%
# Effect of effort 
nI <- length(test$beta_det[,1])
length(which(test$beta_det[,1] < 0))/nI



# Effect of effort 
nI <- length(test$beta_det[,4])
length(which(test$beta_det[,4] < 0))/nI

