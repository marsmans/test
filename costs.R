#------------------------------------------------
# 
# Costs met vrij onzekere waarden
#
#------------------------------------------------

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

library(ggplot2)
library(sensitivity)
library(lhs)
library(Hmisc)
#library(ks)
library(pse)
library(xtable)

#----------- Relatie cumulatieve CO2 <-> mitigatie kosten -----------------

# data inlezen
costsLL <- read.csv(file = "Databases/costs_LL.txt", header = TRUE)
costsUL <- read.csv(file = "Databases/costs_UL.txt", header = TRUE)

# rechte lijn best fits
gLL <- lm(data = costsLL, mitigation_costs ~ cumuCO2)
gUL <- lm(data = costsUL, mitigation_costs ~ cumuCO2)

# "gemiddelde" lijn
intercept_mean = (coef(gLL)[1] + coef(gUL)[1])/2
slope_mean = (coef(gLL)[2] + coef(gUL)[2])/2

costs_mean <- slope_mean

# std van de hellingshoek
# Als onzekerheidsrange [10%,90%] is
costs.std90 <- (coef(gUL)[2] - slope_mean)/abs(qnorm(0.90))
costs.std10 <- (slope_mean - coef(gLL)[2])/abs(qnorm(0.10))
costs.std = (costs.std10 + costs.std90)/2

# Als onzekerheidsrange [5%,95%] is
costs.std95 <- (coef(gUL)[2] - slope_mean)/abs(qnorm(0.95))
costs.std05 <- (slope_mean - coef(gLL)[2])/abs(qnorm(0.05))
costs.std2 = (costs.std05 + costs.std95)/2




