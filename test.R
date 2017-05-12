# dit is een test voor github
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

library(ggplot2)
library(sensitivity)

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

runs <- 100000
a.samples <- rbeta(runs,20,100)
b.samples <- rbeta(runs,38,110)
a.samples
plot(a.samples)
hist(a.samples)
hist(b.samples)


#------------- MC ----------------------

runs <- 100000

#------------- Temp 2010 --------------------------
temp2010 <- read.csv(file = "Databases/temp2010.txt", header = TRUE)
T2010mean <- with(temp2010,temp)[1]

# [10%,90%]
T2010std90 <- (with(temp2010,temp)[2] - with(temp2010,temp)[1])/abs(qnorm(0.90))
T2010std10 <- (with(temp2010,temp)[1] - with(temp2010,temp)[3])/abs(qnorm(0.10))
T2010std = (T2010std10 + T2010std90)/2

# [5%,95%]
T2010std95 <- (with(temp2010,temp)[2] - with(temp2010,temp)[1])/abs(qnorm(0.95))
T2010std05 <- (with(temp2010,temp)[1] - with(temp2010,temp)[3])/abs(qnorm(0.05))
T2010std2 = (T2010std05 + T2010std95)/2

T2010 <- rnorm(1,mean = T2010mean, sd = T2010std)

#------------ CumuCO2 2010 ------------------------
cumuCO22010 <- read.csv(file = "Databases/cumuCO22010.txt", header = TRUE)
CO22010mean <- 3.67 * with(cumuCO22010,cumuCO2)[1]

# [10%,90%]
CO22010std90 <- (with(cumuCO22010,cumuCO2)[2] - with(cumuCO22010,cumuCO2)[1])/abs(qnorm(0.90))
CO22010std10 <- (with(cumuCO22010,cumuCO2)[1] - with(cumuCO22010,cumuCO2)[3])/abs(qnorm(0.10))
CO22010std = 3.67 * (CO22010std10 + CO22010std90)/2

# [5%,95%]
CO22010std95 <- (with(cumuCO22010,cumuCO2)[2] - with(cumuCO22010,cumuCO2)[1])/abs(qnorm(0.95))
CO22010std05 <- (with(cumuCO22010,cumuCO2)[1] - with(cumuCO22010,cumuCO2)[3])/abs(qnorm(0.05))
CO22010std2 = 3.67 * (CO22010std05 + CO22010std95)/2

CO22010 <- rnorm(1,mean = CO22010mean, sd = CO22010std)

#----------
  
CS <- slope

cumuCO2em <- function(Ttarget){
  iterations <- 1
  T2010 <- rnorm(iterations, mean = T2010mean, sd = T2010std)
  CO22010 <- rnorm(iterations, mean = CO22010mean, sd = CO22010std)
  return(CO22010 + (Ttarget-T2010)/CS)
}
cumuCO2em(2)
cumuCO2em(1.5)

T2010


#---------- Iets anders, meer gestructureerd, met LHS

require(lhs-package)

factors <- c("T2010", "CS", "CO22010")
q <- c("qnorm", "qnorm","qnorm")
# q.arg <- list

# Zie http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
