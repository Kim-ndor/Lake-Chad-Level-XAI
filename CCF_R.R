# Reading the dataset in R
lake = read.csv('C:\\Users\\Public\\XAI\\LakeXAI.csv', header = TRUE)
 # Installing/calling packages/libraries
 library(jtools)
 library(ISLR)
 library(MASS)

# Getting the working directory
 getwd()
 
 # Setting the working directory

setwd("C:/Users/Public/XAI")

          
# Setting the random seedto make the results reproducible
 
set.seed(1)

# Exploring the dataset 

summary(lake) # Summary

# Correlation analysis
# Cor with p-values for the whole dataset

  # install.packages("Hmisc")
   library(Hmisc)
 
lake = (lake[,2:9])
 
rp_values = rcorr(as.matrix(lake))
rp_values

# Combination of correlation coef. r and correlation test rho

#install.packages("correlation")

library(correlation)

# Correlation Matrix Auto Method
  
rpvalues = correlation::correlation(lake, include_factors = TRUE, method = "auto")

rpvalues

# Creating a report of the correlation test
#install.packages("remotes")
#install.packages("report")
#remotes::install_github("easystats/report")
library(report)
# R_LL and G_LL
LLRG = cor.test(lake$R_LL, lake$G_LL)
LLRG
report(LLRG)

# LL_R and climate variables

# E_R
LLRER = cor.test(lake$R_LL, lake$R_E)
report(LLRER)

# E_G
LLREG = cor.test(lake$R_LL, lake$G_E)
report(LLREG)

# AT_R
LLRATR = cor.test(lake$R_LL, lake$R_AT)
report(LLRATR)

# AT_G
LLRATG = cor.test(lake$R_LL, lake$G_AT)
report(LLRATG)

# P_R
LLRPR = cor.test(lake$R_LL, lake$R_P)
report(LLRPR)

# P_G
LLRPG = cor.test(lake$R_LL, lake$G_P)
report(LLRPG)


#############G_LL and climate variables##################
 # LL_G and climate variables

# E_R
LLGER = cor.test(lake$G_LL, lake$R_E)
report(LLGER)

# E_G
LLGEG = cor.test(lake$G_LL, lake$G_E)
report(LLGEG)

# AT_R
LLGATR = cor.test(lake$G_LL, lake$R_AT)
report(LLGATR)

# AT_G
LLGATG = cor.test(lake$G_LL, lake$G_AT)
report(LLGATG)

# P_R
LLGPR = cor.test(lake$G_LL, lake$R_P)
report(LLGPR)

# P_G
LLGPG = cor.test(lake$G_LL, lake$G_P)
report(LLRPG)

# Time series plot
lake_t = (lake[,2:8])
fix(lake_t)
tslake = ts(lake_t, frequency = 12)
plot.ts(tslake)



#### Cross-correlation analysis##########

############### P and LL ######################

########### R_P ###############

######CCF R_P and R_LL ####################

ccf(lake$R_P, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$R_P, lake$R_LL)

ccfvalues

#### Plotting the lagged r values
# install.packages("astsa")
library('astsa')

lag2.plot(lake$R_P, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF R_P and G_LL ####################

ccf(lake$R_P, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$R_P, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$R_P, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)

############ P_G ###############

######CCF G_P and R_LL ####################

ccf(lake$G_P, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$G_P, lake$R_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_P, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF G_P and G_LL ####################

ccf(lake$G_P, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$G_P, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_P, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)




#################### AT and LL ####################


########### AT_R ###############

######CCF R_AT and LL_R ####################

ccf(lake$AT_R, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$R_AT, lake$R_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$R_AT, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF R_AT and LL_G ####################

ccf(lake$R_AT, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$R_AT, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$R_AT, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)

############ G_AT ###############

######CCF G_AT and LL_R ####################

ccf(lake$G_AT, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$G_AT, lake$R_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_AT, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF G_AT and G_LL ####################

ccf(lake$G_AT, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$G_AT, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_AT, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)


############################ E and LL #############################



############### E_R and LL ######################

########### R_E ###############

######CCF R_E and R_LL ####################

ccf(lake$R_E, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$R_E, lake$R_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$R_E, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF R_E and G_LL ####################

ccf(lake$R_E, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$R_E, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$R_E, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)

############ G_E ###############

######CCF E_G and R_LL ####################

ccf(lake$G_E, lake$R_LL)

# ccf values
ccfvalues = ccf(lake$G_E, lake$R_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_E, lake$R_LL, 5, pch = 20, cex=3, lwl = 2)


#####CCF G_E and G_LL ####################

ccf(lake$G_E, lake$G_LL)

# ccf values
ccfvalues = ccf(lake$G_E, lake$G_LL)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$G_E, lake$G_LL, 5, pch = 20, cex=3, lwl = 2)



          
          

