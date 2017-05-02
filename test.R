# dit is een test voor github
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/")

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
