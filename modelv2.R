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
library(xtable)

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

#----------- Define model ---------------------

oneRun <- function(Ttarget,T2010,TCRE,CO22010) {
  return(CO22010 + (Ttarget - T2010)/TCRE)
}

modelRun <- function (my.data) {
  return(mapply(oneRun, my.data[,1], my.data[,2], my.data[,3], my.data[,4]))
}


#---------- Model run met LHS uit package pse -----------
# require(lhs)

factors <- c("Ttarget","T2010", "TCRE", "CO22010")
q <- c("qunif","qnorm", "qnorm","qnorm")
q.arg <- list(list(min=1,max=4), list(mean=T2010mean, sd=T2010std), list(mean=TCREmean,sd=TCREstd), list(mean=CO22010mean, sd=CO22010std))



#--------------- Andere manier, handmatig  ---------

# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
# en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/

require(lhs)
N <- 1000
# maak "random" LHS
set.seed(21)
x <- randomLHS(N, 4)
# geef namen
colnames(x) <- c("Ttarget", "T2010", "TCRE", "CO22010")

# transformeer random LHS naar LHS met goede parameters
y <- x


# Ttarget verschillend
Ttarget.14 <- qunif(x[,1], min=1,max=4)
Ttarget.1.5 <- qunif(x[,1], min=1.5,max=1.5)
Ttarget.2 <- qunif(x[,1], min=2,max=2)
Ttarget.3 <- qunif(x[,1], min=3,max=3)

T2010 <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
TCRE <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
CO22010 <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)

# run model
cumuCO2result.14 <- mapply(oneRun, Ttarget.14, T2010, TCRE, CO22010)
cumuCO2result.1.5 <- mapply(oneRun, Ttarget.1.5, T2010, TCRE, CO22010)
cumuCO2result.2 <- mapply(oneRun, Ttarget.2, T2010, TCRE, CO22010)
cumuCO2result.3 <- mapply(oneRun, Ttarget.3, T2010, TCRE, CO22010)

# bundel data en resultaat
z.14 <- cbind(Ttarget.14, T2010, TCRE, CO22010, cumuCO2result.14)
z.1.5 <- cbind(Ttarget.1.5, T2010, TCRE, CO22010, cumuCO2result.1.5)
z.2 <- cbind(Ttarget.2, T2010, TCRE, CO22010, cumuCO2result.2)
z.3 <- cbind(Ttarget.3, T2010, TCRE, CO22010, cumuCO2result.3)

# maak plaatjes

# histogram van input
par(mfrow=c(2,2))
hist(Ttarget.14, breaks = "Scott", main = "Histogram of Ttarget", xlab = "Ttarget")
hist(T2010, breaks = "Scott", main = "Histogram of T2010", xlab = "T2010")
hist(TCRE, breaks = "Scott", main = "Histogram of TCRE", xlab = "TCRE")
hist(CO22010, breaks = "Scott", main = "Histogram of CO22010", xlab = "cumuCO22010")
par(mfrow=c(1,1))

# histogrammen van output
par(mfrow=c(2,2))
hist(cumuCO2result.14, breaks = "Scott", main = "CO2result, Ttarget = 1-4", xlab = "cumuCO2")
hist(cumuCO2result.1.5, breaks = "Scott", main = "CO2result, Ttarget = 1.5", xlab = "cumuCO2")
hist(cumuCO2result.2, breaks = "Scott", main = "CO2result, Ttarget = 2", xlab = "cumuCO2")
hist(cumuCO2result.3, breaks = "Scott", main = "CO2result, Ttarget = 3", xlab = "cumuCO2")


# scatterplots:
# 1-4
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.14 <- plot(cumuCO2result.14~Ttarget.14, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.14 <- plot(cumuCO2result.14~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.14 <- plot(cumuCO2result.14~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.14 <- plot(cumuCO2result.14~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=1-4", outer=TRUE)

# 1.5
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.1.5 <- plot(cumuCO2result.1.5~Ttarget.1.5, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.1.5 <- plot(cumuCO2result.1.5~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.1.5 <- plot(cumuCO2result.1.5~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.1.5 <- plot(cumuCO2result.1.5~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=1.5", outer=TRUE)

# 2
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.2 <- plot(cumuCO2result.2~Ttarget.2, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.2 <- plot(cumuCO2result.2~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.2 <- plot(cumuCO2result.2~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.2 <- plot(cumuCO2result.2~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=2", outer=TRUE)

# 3
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.3 <- plot(cumuCO2result.3~Ttarget.3, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.3 <- plot(cumuCO2result.3~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.3 <- plot(cumuCO2result.3~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.3 <- plot(cumuCO2result.3~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-Co2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=1000, Ttarget=3", outer=TRUE)


# density function
d.14 <- density(cumuCO2result.14)
plot(d.14)

# correlation coefficient matrix
CCmatrix.14 <- cor(z.14)
CCmatrix.1.5 <- cor(z.1.5)
CCmatrix.2 <- cor(z.2)
CCmatrix.3 <- cor(z.3)

# bundel alle CC waarden
CCmatrix <- rbind(CCmatrix.14[,5], CCmatrix.1.5[,5], CCmatrix.2[,5], CCmatrix.3[,5])
CCmatrix <- CCmatrix[,-5]
rownames(CCmatrix) <- c("1-4", "1.5", "2", "3")

# print CCmatrix naar latex code
# stargazer(CCmatrix, type = "text", out = "out.txt") was mooi geweest
xtable(CCmatrix, digits = 3)




