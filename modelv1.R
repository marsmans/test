#-------------------------------------------
# 
# Dummymodel met vrij onzeker waarden
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

# library(ggplot2)
library(sensitivity)
library(lhs)
#library(Hmisc)
#library(ks)
library(pse)

#----------- Relatie cumulatieve CO2 <-> temperatuur -----------------

# data inlezen
cumuvstempLL <- read.csv(file = "Databases/cumuvstemp_lowerlimit.txt", header = TRUE)
cumuvstempUL <- read.csv(file = "Databases/cumuvstemp_upperlimit.txt", header = TRUE)

# rechte lijn best fits
fLL <- lm(data = cumuvstempLL, temp ~ cumuCO2)
fUL <- lm(data = cumuvstempUL, temp ~ cumuCO2)

# "gemiddelde" lijn
intercept = (coef(fLL)[1] + coef(fUL)[1])/2
slope = (coef(fLL)[2] + coef(fUL)[2])/2

TCREmean <- slope

# Als onzekerheidsrange [10%,90%] is
TCREstd90 <- (coef(fUL)[2] - slope)/abs(qnorm(0.90))
TCREstd10 <- (slope - coef(fLL)[2])/abs(qnorm(0.10))
TCREstd = (TCREstd10 + TCREstd90)/2

# Als onzekerheidsrange [5%,95%] is
TCREstd95 <- (coef(fUL)[2] - slope)/abs(qnorm(0.95))
TCREstd05 <- (slope - coef(fLL)[2])/abs(qnorm(0.05))
TCREstd2 = (TCREstd05 + TCREstd95)/2

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

# T2010 <- rnorm(1,mean = T2010mean, sd = T2010std)

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

#---------- Model run met LHS uit package pse -------

# require(lhs)

factors <- c("Ttarget","T2010", "TCRE", "CO22010")
q <- c("qunif","qnorm", "qnorm","qnorm")
q.arg <- list(list(min=1,max=4), list(mean=T2010mean, sd=T2010std), list(mean=TCREmean,sd=TCREstd), list(mean=CO22010mean, sd=CO22010std))


oneRun <- function(Ttarget,T2010,TCRE,CO22010) {
  return(CO22010 + (Ttarget - T2010)/TCRE)
}

modelRun <- function (my.data) {
  return(mapply(oneRun, my.data[,1], my.data[,2], my.data[,3], my.data[,4]))
}

myLHS <- LHS(modelRun, factors, 200, q, q.arg, nboot=50)

myLHS.data <- get.data(myLHS)
myLHS.results <- get.results(myLHS)

newLHS <- LHS(modelRun, factors, 1000, q, q.arg)
(mySmba <- sbma(myLHS,newLHS))

par(mfrow=c(1,1))


#------------------ Probeersels met sensitivity package ----------

# Werkt nog niet echt lekker ofzo
x <- fast99(model = NULL, factors = 3, n = 1000, q = "qunif", q.arg = list(min = -pi, max = pi))
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)



# Example of use of fast99 with "model = NULL"
x <- fast99(model = modelRun, factors, n = 200,q, q.arg)
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)





#--------------- Andere manier, zonder pse ---------

# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html

require(lhs)
N <- 10000
x <- randomLHS(N, 4)
y <- x
y[,1] <- qunif(x[,1], min=1,max=4)
y[,2] <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
y[,3] <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
y[,4] <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

oldpar <- par()
par(mfrow=c(2,2))
apply(x, 2, hist)

par(mfrow=c(2,2))
apply(y, 2, hist)
par(oldpar)

result<-mapply(oneRun, y[,1], y[,2], y[,3], y[,4])
hist(result)

d <- density(result)
plot(d)

fast99(model = modelRun, factors = factors, 200, q, q.arg)

data.frame(matrix(runif(3*n, min = 0.1, max = 2), nrow=n))

# test
factorstest <- c("x1", "x2", "x3")
factorstest2 <- c("x1", "x2", "x3","x4")

qtest <- c("qunif", "qnorm", "qunif")
qtest2 <- c("qunif", "qnorm", "qnorm", "qnorm")

q.argtest <- list(list(min=1,max=4), list(mean=T2010mean, sd=T2010std), list(min=1,max=4))
q.argtest2 <- list(list(min=1,max=4), list(mean=T2010mean, sd=T2010std), list(mean=TCREmean,sd=TCREstd), list(mean=CO22010mean, sd=CO22010std))

modelRuntest <- function (Input) {
  (Input[,1]-0.5)*2 + (Input[,2]+1)*5 + (Input[,3]-0.2)*3
}

modelRuntest2 <- function (Input) {
  (Input[,1]-0.5)*2 + (Input[,2]+1)*5 + (Input[,3]-0.2)*3 + (Input[,4]-0.2)*3
}

test <- fast99(modelRuntest, factorstest, n = 1000, q = qtest,  q.arg = q.argtest )
test2 <- fast99(modelRuntest2, factorstest2, n = 1000, q = qtest2,  q.arg = q.argtest2)

x <- fast99(modelRun, factors, n = 1000, q = q, q.arg = q.arg)
x <- fast99(model = modelRun, factors, n = 200,q, q.arg)

test
test2

modelRuntest2
factorstest2
qtest2
q.argtest2

modelRun
factors
q
q.arg
