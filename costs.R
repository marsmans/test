#------------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in costs
#
#------------------------------------------------

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

if(!exists("foo", mode="function")) source("test/packages.R")
source("test/TCRE.R")


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
costs.std = abs((costs.std10 + costs.std90)/2)

# Als onzekerheidsrange [5%,95%] is
costs.std95 <- (coef(gUL)[2] - slope_mean)/abs(qnorm(0.95))
costs.std05 <- (slope_mean - coef(gLL)[2])/abs(qnorm(0.05))
costs.std2 = abs((costs.std05 + costs.std95)/2)


#----------- intercept ------------------------

# cost = 0 => cumuCO2 = baselineCO2
# =>
# intercept = -baselineCO2 * slope


#----------- Define model ---------------------

costs.oneRun <- function(cumuCO2,costs.slope,baselineCO2) {
  return(costs.slope * (cumuCO2 - baselineCO2))
}

cost.oneRun2 <- function(cumuCO2,cost.slope) {
  return(cost.slope * cumuCO2 + (-1 *baselineCO2 * cost.slope))
}



#--------------- Sample cost.slope en baselineCO2  ---------


require(lhs)
N <- 10000
# maak "random" LHS
set.seed(21)
costs.x <- randomLHS(N, 2)
# geef namen
colnames(costs.x) <- c("cost.slope","baselineCO2")

# transformeer random LHS naar LHS met goede parameters

costs.slope <- qnorm(costs.x[,1], mean=costs_mean, sd=costs.std)
costs.slope <- qpert(costs.x[,1], coef(gUL)[2], costs_mean, coef(gLL)[2], shape = 4)
baselineCO2 <- qunif(costs.x[,2], min=6000,max=6000)

#---------- run model -----------

costs.result.14 <- mapply(costs.oneRun, cumuCO2result.14, costs.slope, baselineCO2)
costs.result.1.5 <- mapply(costs.oneRun, cumuCO2result.1.5, costs.slope, baselineCO2)
costs.result.2 <- mapply(costs.oneRun, cumuCO2result.2, costs.slope, baselineCO2)
costs.result.3 <- mapply(costs.oneRun, cumuCO2result.3, costs.slope, baselineCO2)


# bundel data en resultaat
costs.z.14 <- cbind(z.14, costs.slope, baselineCO2, costs.result.14)
costs.z.1.5 <- cbind(z.1.5, costs.slope, baselineCO2, costs.result.1.5)
costs.z.2 <- cbind(z.2, costs.slope, baselineCO2, costs.result.2)
costs.z.3 <- cbind(z.3, costs.slope, baselineCO2, costs.result.3)


#-------- correlation coefficient matrix -----------

costs.CCmatrix.14 <- cor(costs.z.14)
costs.CCmatrix.1.5 <- cor(costs.z.1.5)
costs.CCmatrix.2 <- cor(costs.z.2)
costs.CCmatrix.3 <- cor(costs.z.3)

# bundel alle CC waarden
costs.CCmatrix <- rbind(costs.CCmatrix.14[,8], costs.CCmatrix.1.5[,8], costs.CCmatrix.2[,8], costs.CCmatrix.3[,8])
costs.CCmatrix <- costs.CCmatrix[,-8]
rownames(costs.CCmatrix) <- c("1-4", "1.5", "2", "3")


