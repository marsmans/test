# dit is een test voor github
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

library(ggplot2)

#----------- Relatie cumulatieve CO2 <-> temperatuur -----------------

# data inlezen
cumuvstempLL <- read.csv(file = "Databases/cumuvstemp_lowerlimit.txt", header = TRUE)
cumuvstempUL <- read.csv(file = "Databases/cumuvstemp_upperlimit.txt", header = TRUE)

# plaatje
plot(cumuvstempLL,xlim=c(0,9000))
# rechte lijn best fit
fLL <- lm(data = cumuvstempLL, temp ~ cumuCO2)
abline(fLL)

# plaatje
points(cumuvstempUL,xlim=c(0,9000))
# rechte lijn best fit
fUL <- lm(data = cumuvstempUL, temp ~ cumuCO2)
abline(fUL)


#----------- Relatie cumulatieve CO2 <-> kosten -------------------

# data inlezen
cumuvscostsGE <- read.csv(file = "Databases/cumuCO2vscostsGEmodels.txt", header = TRUE)
cumuvscostsPE <- read.csv(file = "Databases/cumuCO2vscostsPEmodels.txt", header = TRUE)
cumuvscostsAllE <- read.csv(file = "Databases/cumuCO2vscostsAllEmodels.txt", header = TRUE)

ggplot(cumuvscostsAllE, aes(cumuCO2,costs)) +
  geom_point() +
  geom_smooth()

ggplot(cumuvscostsGE, aes(cumuCO2,costs)) +
  geom_point() +
  geom_smooth()

ggplot(cumuvscostsPE, aes(cumuCO2,costs)) +
  geom_point() +
  geom_smooth()
