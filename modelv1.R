#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

library(ggplot2)
library(sensitivity)
library(lhs)
library(Hmisc)
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

#------------ Schrijf naar file ------------

parameters <- c(paste("T2010mean = ",T2010mean),paste("T2010sd(10-90) = ",T2010std),paste("T2010sd2(5-95) = ",T2010std2),
                paste("CO22010mean = ",CO22010mean),paste("CO22010sd(10-90) = ",CO22010std),paste("CO22010sd2(5-95) = ",CO22010std2),
                paste("TCREmean = ",TCREmean),paste("TCREsd(10-90) = ",TCREstd),paste("TCREsd2(5-95) = ",TCREstd2))

# write(parameters, "parameters.txt", append = FALSE, sep = "\n")

fileConn <- file("test/parameters.txt")
writeLines(parameters, fileConn)
close(fileConn)

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

# scatterplots:
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot <- plot(myLHS.results~myLHS.data[,1], sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot <- plot(myLHS.results~myLHS.data[,2], sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot <- plot(myLHS.results~myLHS.data[,3], sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot <- plot(myLHS.results~myLHS.data[,4], sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=200, T2010=1-4", outer=TRUE)

#------------------ Probeersels met sensitivity package ----------

# Werkt nog niet echt lekker ofzo
x <- fast99(model = NULL, factors = 3, n = 1000, q = "qunif", q.arg = list(min = -pi, max = pi))
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)


# Example of use of fast99 with "model = NULL"
x <- fast99(model = modelRun, factors, n = 200, q, q.arg)
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)



#--------------- Andere manier, zonder pse ---------

# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
# en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/

require(lhs)
N <- 1000
# maak random LHS
set.seed(21)
x <- randomLHS(N, 4)
# geef namen
colnames(x) <- c("Ttarget", "T2010", "TCRE", "CO22010")

# transformeer random LHS naar LHS met goede parameters
# T2010 = 1-4
y.14 <- x
set.seed(21)
y.14[,1] <- qunif(x[,1], min=1,max=4)
y.14[,2] <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
y.14[,3] <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
y.14[,4] <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

# run model
cumuCO2result.14 <- mapply(oneRun, y.14[,1], y.14[,2], y.14[,3], y.14[,4])

# bundel data en resultaat
z.14 <- cbind(y.14, cumuCO2result.14)

# maak plaatjes
# histogrammen van input
oldpar <- par()
par(mfrow=c(2,2))
apply(x, 2, hist)

par(mfrow=c(2,2))
apply(y.14, 2, hist)
par(oldpar)

hist(cumuCO2result.14)
par(mfrow=c(2,2))

#apply(y,2, function (y) {plot(cumuCO2result~y)})

# scatterplots:
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.14 <- plot(cumuCO2result.14~y.14[,1], sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.14 <- plot(cumuCO2result.14~y.14[,2], sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.14 <- plot(cumuCO2result.14~y.14[,3], sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.14 <- plot(cumuCO2result.14~y.14[,4], sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=1-4", outer=TRUE)

# density function
d.14 <- density(cumuCO2result.14)
plot(d.14)

# correlation coefficient matrix
CCmatrix.14 <- cor(z.14)


# T2010 = 1.5
y.1.5 <- x
set.seed(21)
y.1.5[,1] <- qunif(x[,1], min=1.5,max=1.5)
y.1.5[,2] <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
y.1.5[,3] <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
y.1.5[,4] <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

# run model
cumuCO2result.1.5 <- mapply(oneRun, y.1.5[,1], y.1.5[,2], y.1.5[,3], y.1.5[,4])

# bundel data en resultaat
z.1.5 <- cbind(y.1.5, cumuCO2result.1.5)

# maak plaatjes
oldpar <- par()

#histogram van input
par(mfrow=c(2,2))
apply(y.1.5, 2, hist)
par(oldpar)

hist(cumuCO2result.1.5)
par(mfrow=c(2,2))

#apply(y,2, function (y) {plot(cumuCO2result~y)})

# scatterplots:
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.1.5 <- plot(cumuCO2result.1.5~y.1.5[,1], sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.1.5 <- plot(cumuCO2result.1.5~y.1.5[,2], sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.1.5 <- plot(cumuCO2result.1.5~y.1.5[,3], sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.1.5 <- plot(cumuCO2result.1.5~y.1.5[,4], sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=1.5", outer=TRUE)

# correlation coefficient matrix
CCmatrix.1.5 <- cor(z.1.5)

# density function
d.1.5 <- density(cumuCO2result.1.5)
plot(d.1.5)



# T2010 = 2
y.2 <- x
set.seed(21)
y.2[,1] <- qunif(x[,1], min=2,max=2)
y.2[,2] <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
y.2[,3] <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
y.2[,4] <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

# run model
cumuCO2result.2 <- mapply(oneRun, y.2[,1], y.2[,2], y.2[,3], y.2[,4])

# bundel data en resultaat
z.2 <- cbind(y.2, cumuCO2result.2)

# maak plaatjes
oldpar <- par()

#histogram van input
par(mfrow=c(2,2))
apply(y.2, 2, hist)
par(oldpar)

hist(cumuCO2result.2)
par(mfrow=c(2,2))

#apply(y,2, function (y) {plot(cumuCO2result~y)})

# scatterplots:
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.2 <- plot(cumuCO2result.2~y.2[,1], sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.2 <- plot(cumuCO2result.2~y.2[,2], sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.2 <- plot(cumuCO2result.2~y.2[,3], sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.2 <- plot(cumuCO2result.2~y.2[,4], sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=2", outer=TRUE)

# correlation coefficient matrix
CCmatrix.2 <- cor(z.2)

# density function
d.2 <- density(cumuCO2result.2)
plot(d.2)




# T2010 = 3
y.3 <- x
set.seed(21)
y.3[,1] <- qunif(x[,1], min=3,max=3)
y.3[,2] <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
y.3[,3] <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
y.3[,4] <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

# run model
cumuCO2result.3 <- mapply(oneRun, y.3[,1], y.3[,2], y.3[,3], y.3[,4])

# bundel data en resultaat
z.3 <- cbind(y.3, cumuCO2result.3)

# maak plaatjes
oldpar <- par()

#histogram van input
par(mfrow=c(2,2))
apply(y.2, 2, hist)
par(oldpar)

hist(cumuCO2result.3)
par(mfrow=c(2,2))

#apply(y,2, function (y) {plot(cumuCO2result~y)})

# scatterplots:
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.3 <- plot(cumuCO2result.3~y.3[,1], sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.3 <- plot(cumuCO2result.3~y.3[,2], sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.3 <- plot(cumuCO2result.3~y.3[,3], sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.3 <- plot(cumuCO2result.3~y.3[,4], sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=3", outer=TRUE)

# correlation coefficient matrix
CCmatrix.3 <- cor(z.3)

# density function
d.3 <- density(cumuCO2result.3)
plot(d.3)



#---------- fast99 (?) -------
fast99(model = modelRun, factors = factors, 200, q, q.arg) # werkt niet

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

x <- fast99(modelRun, factors, n = 1000, q = q, q.arg = q.arg) # werkt wel
x <- fast99(model = modelRun, factors, n = 2000, q = q, q.arg = q.arg)       # werkt niet... ? wel... ?
x

