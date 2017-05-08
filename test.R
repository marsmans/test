# dit is een test voor github
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

library(ggplot2)

#----------- Relatie cumulatieve CO2 <-> temperatuur -----------------

# data inlezen
cumuvstempLL <- read.csv(file = "Databases/cumuvstemp_lowerlimit.txt", header = TRUE)
cumuvstempUL <- read.csv(file = "Databases/cumuvstemp_upperlimit.txt", header = TRUE)

# plaatje
plot(cumuvstempLL,xlim=c(0,9))
# rechte lijn best fit
fLL <- lm(data = cumuvstempLL, temp ~ cumuCO2)
abline(fLL)

# plaatje
points(cumuvstempUL,xlim=c(0,9000))
# rechte lijn best fit
fUL <- lm(data = cumuvstempUL, temp ~ cumuCO2)
abline(fUL)

# gemiddelde lijn
intercept = (coef(fLL)[1] + coef(fUL)[1])/2
slope = (coef(fLL)[2] + coef(fUL)[2])/2
abline(intercept, slope)



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


#------------- Monte Carlo tests ---------------------

runs <- 100000
one.trial <- function(){sum(sample(c(0,1),10,replace=T)) >3}
reps <- replicate(runs,one.trial())
mc.binom <- sum(reps)/runs
mc.binom

runs <- 100
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
sum(in.circle)

days <- 200
changes <- rnorm(days,mean=1.001,sd=0.005)
changes
cumprod(c(20,changes))
plot(cumprod(c(20,changes)),type='l')
