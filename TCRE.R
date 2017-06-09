#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in TCRE
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")


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


#----------- Define model ---------------------

oneRun <- function(Ttarget,T2010,TCRE,CO22010) {
  return(CO22010 + (Ttarget - T2010)/TCRE)
}

modelRun <- function (my.data) {
  return(mapply(oneRun, my.data[,1], my.data[,2], my.data[,3], my.data[,4]))
}


#---------- Model run met LHS uit package pse -----------
# require(lhs)

#factors <- c("Ttarget","T2010", "TCRE", "CO22010")
#q <- c("qunif","qnorm", "qnorm","qnorm")
#q.arg <- list(list(min=1,max=4), list(mean=T2010mean, sd=T2010std), list(mean=TCREmean,sd=TCREstd), list(mean=CO22010mean, sd=CO22010std))



#--------------- Andere manier, handmatig  ---------

# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
# en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/

require(lhs)
N <- 10000
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


# density function
d.14 <- density(cumuCO2result.14)

# correlation coefficient matrix
CCmatrix.14 <- cor(z.14)
CCmatrix.1.5 <- cor(z.1.5)
CCmatrix.2 <- cor(z.2)
CCmatrix.3 <- cor(z.3)

# bundel alle CC waarden
CCmatrix <- rbind(CCmatrix.14[,5], CCmatrix.1.5[,5], CCmatrix.2[,5], CCmatrix.3[,5])
CCmatrix <- CCmatrix[,-5]
rownames(CCmatrix) <- c("1-4", "1.5", "2", "3")



