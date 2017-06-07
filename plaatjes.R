#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Plaatjes
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 


# maak plaatjes

# plaatje van TCRE
par(mfrow=c(1,1))
plot(cumuvstempLL,xlim=c(0,9000))
abline(fLL)
points(cumuvstempUL,xlim=c(0,9000))
abline(fUL)
abline(intercept, slope)

# plaatje van costs
par(mfrow=c(1,1))
plot(costsLL,ylim=c(0,8))
abline(gLL)
points(costsUL)
abline(gUL)
abline(intercept_mean, slope_mean)


# histogram van input
par(mfrow=c(2,2))
hist(Ttarget.14, breaks = "Scott", main = "Histogram of Ttarget", xlab = "Ttarget")
hist(T2010, breaks = "Scott", main = "Histogram of T2010", xlab = "T2010")
hist(TCRE, breaks = "Scott", main = "Histogram of TCRE", xlab = "TCRE (degrees Celsius/tCO2")
hist(CO22010, breaks = "Scott", main = "Histogram of CO22010", xlab = "cumuCO22010")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(costs.slope, breaks = "Scott", main = "Histogram of costs.slope", xlab = "cost respons to emissions (%pt/tCO2)")
hist(baselineCO2, breaks = "Scott", main = "Histogram of baseline CO2", xlab = "Baseline CO2")
par(mfrow=c(1,1))


# histogrammen van CO2result
par(mfrow=c(2,2))
hist(cumuCO2result.14, breaks = "Scott", main = "CO2result, Ttarget = 1-4", xlab = "cumuCO2")
hist(cumuCO2result.1.5, breaks = "Scott", main = "CO2result, Ttarget = 1.5", xlab = "cumuCO2")
hist(cumuCO2result.2, breaks = "Scott", main = "CO2result, Ttarget = 2", xlab = "cumuCO2")
hist(cumuCO2result.3, breaks = "Scott", main = "CO2result, Ttarget = 3", xlab = "cumuCO2")

# histogrammen van costs.result
par(mfrow=c(2,2))
hist(costs.result.14, breaks = "Scott", main = "costs.result, Ttarget = 1-4", xlab = "Costs")
hist(costs.result.1.5, breaks = "Scott", main = "costs.result, Ttarget = 1.5", xlab = "Costs")
hist(costs.result.2, breaks = "Scott", main = "costs.result, Ttarget = 2", xlab = "Costs")
hist(costs.result.3, breaks = "Scott", main = "costs.result, Ttarget = 3", xlab = "Costs")




# scatterplots van input vs cumuCOresult
# 1-4
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.14 <- plot(cumuCO2result.14~Ttarget.14, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.14 <- plot(cumuCO2result.14~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.14 <- plot(cumuCO2result.14~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.14 <- plot(cumuCO2result.14~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1-4", outer=TRUE)

# 1.5
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.1.5 <- plot(cumuCO2result.1.5~Ttarget.1.5, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.1.5 <- plot(cumuCO2result.1.5~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.1.5 <- plot(cumuCO2result.1.5~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.1.5 <- plot(cumuCO2result.1.5~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1.5", outer=TRUE)

# 2
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.2 <- plot(cumuCO2result.2~Ttarget.2, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.2 <- plot(cumuCO2result.2~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.2 <- plot(cumuCO2result.2~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.2 <- plot(cumuCO2result.2~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=2", outer=TRUE)

# 3
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.3 <- plot(cumuCO2result.3~Ttarget.3, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.3 <- plot(cumuCO2result.3~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.3 <- plot(cumuCO2result.3~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.3 <- plot(cumuCO2result.3~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=3", outer=TRUE)


# scatterplots van input vs costs.result
# 1-4
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.14 <- plot(costs.result.14~Ttarget.14, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.14 <- plot(costs.result.14~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.14 <- plot(costs.result.14~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.14 <- plot(costs.result.14~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.14 <- plot(costs.result.14~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.14 <- plot(costs.result.14~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1-4", outer=TRUE)

# 1.5
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.1.5 <- plot(costs.result.1.5~Ttarget.1.5, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.1.5 <- plot(costs.result.1.5~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.1.5 <- plot(costs.result.1.5~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.1.5 <- plot(costs.result.1.5~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.1.5 <- plot(costs.result.1.5~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.1.5 <- plot(costs.result.1.5~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1.5", outer=TRUE)

# 2
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.2 <- plot(costs.result.2~Ttarget.2, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.2 <- plot(costs.result.2~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.2 <- plot(costs.result.2~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.2 <- plot(costs.result.2~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.2 <- plot(costs.result.2~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.2 <- plot(costs.result.2~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=2", outer=TRUE)

# 3
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.3 <- plot(costs.result.3~Ttarget.3, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.3 <- plot(costs.result.3~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.3 <- plot(costs.result.3~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.3 <- plot(costs.result.3~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.3 <- plot(costs.result.3~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.3 <- plot(costs.result.3~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=3", outer=TRUE)



# scatterplots van cumuCO2result vs costs
par(mfrow=c(2,2), oma=c(2,0,2,0))
cumuCO2vscosts.plot.14 <- plot(costs.result.14~cumuCO2result.14, sub = "Ttarget=1-4", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.1.5 <- plot(costs.result.1.5~cumuCO2result.1.5, sub = "Ttarget=1.5", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.2 <- plot(costs.result.2~cumuCO2result.2, sub = "Ttarget=2", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.3 <- plot(costs.result.3~cumuCO2result.3, sub = "Ttarget=3", xlab = "cumulative CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000", outer=TRUE)


# density function
d.14 <- density(cumuCO2result.14)
d.1.5 <- density(cumuCO2result.1.5)
d.2 <- density(cumuCO2result.2)
d.3 <- density(cumuCO2result.3)

